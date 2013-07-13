package thu.ailab.template

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{ HashMap => MHashMap }
import scala.annotation.tailrec
import thu.ailab.global.MyConfigFactory
import thu.ailab.sequence._
import thu.ailab.tree.TreeNode
import thu.ailab.distance.LCSWithPath
import thu.ailab.cluster.TSNaiveAggloCluster
import thu.ailab.global.LoggerTrait

/**
 * Build optional nodes using clustering.
 * 
 * Main job is done by the functions of the companion object.
 */
class ClusterMethod(centerId: Int, fileIds: Seq[Int]) {
  private val dataset = MyConfigFactory.getValue[String]("global.dataset") 
  val id2filename = scala.io.Source.fromFile(
    MyConfigFactory.getValue[String](dataset, "output.id2filename")).getLines.toArray
  val tagSeqFactory = new TagSeqFactory(id2filename)
  val docClusterSize = fileIds.size
  /**
   * Get the common sequence of all sequence, naming it "document center"
   */
  val docCenter = ClusterMethod.findLCSInAll(tagSeqFactory.getInstance(centerId),
    fileIds.filter(_ != centerId).iterator,
    tagSeqFactory.getInstance)
  def getTemplateNodeArray() = {
    val tagSegMap = ClusterMethod.getTagSegMap(docCenter,
        fileIds.iterator,
        tagSeqFactory.getInstance)
    val posToOpNode = ClusterMethod.calculate(tagSegMap,
      (c: Int) => 1.0 * c / docClusterSize)
    ClusterMethod.composeTemplateNodeArray(docCenter, tagSegMap, posToOpNode)
  }
}

/**
 * Define various helper functions to build optional nodes.
 * 
 * We use "Actor" to speed up the calculation.
 */
object ClusterMethod extends LoggerTrait {
  import akka.actor._
  import akka.routing.RoundRobinRouter
  import scala.concurrent.duration._

  /**
   * Messages
   */
  sealed trait Message
  case object StartCalculation extends Message
  case class AllFinished(duration: Duration) extends Message
  case class TaskBegins(taskId: Int,
    tss: Array[TagSegment],
    toConfidence: (Int) => Double) extends Message
  case class TaskEnds(taskId: Int, opNodeOption: Option[OptionalNode]) extends Message

  private val dataset = MyConfigFactory.getValue[String]("global.dataset")
  val minConfidence = MyConfigFactory.getValue[Double](dataset, "template.optionalConfidence")

  /**
   * Compose the template node array according to alignment
   * and the mapping between range and TagSegment array.
   */
  def composeTemplateNodeArray(docCenter: TagSequence,
      tagSegMap: MHashMap[(Int, Int), Array[TagSegment]],
      posToOpNode: MHashMap[Int, OptionalNode]) = {    
    val tns = new ArrayBuffer[TemplateNode]
    val sortedPos = posToOpNode.keys.toSeq.sorted
    var prevPos = 0
    /**
     * In every step, we generate one Essential Node and one Optional Node.
     */
    for (pos <- sortedPos) {
      tns += new EssentialNode(docCenter.makeTagSequence((prevPos to pos)))
      tns += posToOpNode(pos)
      prevPos = pos + 1
    }
    /**
     * Bounds checking
     */
    if (prevPos < docCenter.compactLength) {
      tns += new EssentialNode(docCenter.makeTagSequence(
          (prevPos until docCenter.compactLength)))
    }
    tns.toArray
  }
  
  /**
   * Get TagSegment array according to alignment.
   * Return the mapping relation.
   * 
   * The mapping relation: 
   *     key: Range represented by (Int, Int)
   *   value: Array of TagSegment.
   */
  def getTagSegMap(docCenter: TagSequence, 
      idIterator: Iterator[Int],
      getSequence: (Int) => TagSequence) = {
    val tagSegMap = new MHashMap[(Int, Int), Array[TagSegment]]
    val centerLen = docCenter.compactLength
    for (id <- idIterator) {
      val ts = getSequence(id)
      val commonIndices = new LCSWithPath(docCenter, ts).getCommonIndices
      assert(centerLen == commonIndices.length)
      var prevIndex = (-1, -1)
      /**
       * Check if there are unaligned sequences
       */
      for (curIndex <- commonIndices) {
        if (curIndex._2 - prevIndex._2 != 1) {
          val tagSeg = new TagSegment(ts, prevIndex._2, curIndex._2, id)
          val range = (prevIndex._1, curIndex._1)
          tagSegMap(range) = tagSegMap.getOrElse(range, Array()) :+ tagSeg
        }
        prevIndex = curIndex
      }
      /**
       * Bounds checking
       */
      if (ts.compactLength - 1 > prevIndex._2) {
        val tagSeg = new TagSegment(ts, prevIndex._2,
          ts.compactLength, id)
        val range = (prevIndex._1, centerLen)
        tagSegMap(range) = tagSegMap.getOrElse(range, Array()) :+ tagSeg
      }
    }
    tagSegMap
  }
  /**
   * Do clustering!
   */
  def clusterTagSegment(tss: Array[TagSegment],
    toConfidence: (Int) => Double): Option[OptionalNode] = {
    def getSequence(id: Int) = tss(id).getTagSeq
    val naive = new TSNaiveAggloCluster(tss)
    naive.clustering
    val clusters = naive.getClusters
    /**
     * Generate TagSeqBundles which are the basic elements of an Optional Node 
     */
    val bundles =
      (for {
        (centerId, cluster) <- clusters
        confidence = toConfidence(cluster.getPointCount)
        if confidence > minConfidence
      } yield {
        val clcs = findLCSInAll(tss(centerId).getTagSeq,
          cluster.getPoints.map(_.id).filter(_ != centerId).iterator,
          getSequence)
        new TagSeqBundle(clcs, confidence)
      })
    /**
     * Build an Optional Node if necessary
     */
    if (bundles.size > 0) {
      Some(new OptionalNode(bundles.toArray.sortBy(-_.confidence)))
    }
    else None
  }
  /**
   * Run LCS n times and get common sequence of the TagSequence set.
   */
  def findLCSInAll(initTs: TagSequence, idIterator: Iterator[Int],
    getSequence: (Int) => TagSequence) = {
    /**
     * Tail-recursive version to optimize
     */
    @tailrec
    def helper(lcs: TagSequence, it: Iterator[Int]): TagSequence = {
      if (it.hasNext) {
        val id = it.next
        val otherTagSeq = getSequence(id)
        val indices = new LCSWithPath(lcs, otherTagSeq).getCommonIndices.unzip._1
        helper(lcs.makeTagSequence(indices), it)
      } else {
        lcs
      }
    }
    helper(initTs, idIterator)
  }

  /**
   * This will be returned to the outside caller
   */
  private val posToOpNode = new MHashMap[Int, OptionalNode]

  /**
   * The Actor model implementation part begins
   */
  class Worker extends Actor {
    def receive = {
      case TaskBegins(taskId, tss, toConfidence) =>
        val opNodeOption = clusterTagSegment(tss, toConfidence)
        sender ! TaskEnds(taskId, opNodeOption)
    }
  }
  class Master(tagSegMap: MHashMap[(Int, Int), Array[TagSegment]],
    toConfidence: (Int) => Double,
    listener: ActorRef) extends Actor {
    val nrOfWorkers = MyConfigFactory.getValue[Int]("actor.nrOfWorkers")
    val nrOfMessages = tagSegMap.size
    val start = System.currentTimeMillis()
    val workerRouter = context.actorOf(
      Props(new Worker).withRouter(RoundRobinRouter(nrOfWorkers)),
      name = "workRouter")
    var finishedCount = 0
    def receive = {
      case StartCalculation =>
        for ((range, tss) <- tagSegMap) {
          workerRouter ! TaskBegins(range._1, tss, toConfidence)
        }
      case TaskEnds(taskId, opNodeOption) =>
        finishedCount += 1
        logger.info("finishedCount: " + finishedCount)
        if (opNodeOption.isDefined) {
          posToOpNode += taskId -> opNodeOption.get
        }        
        if (finishedCount == nrOfMessages) {
          listener ! AllFinished((System.currentTimeMillis() - start).millis)
          context.stop(self)
        }
    }
  }
  class Listener extends Actor {
    def receive = {
      case AllFinished(duration) =>
        println("Duration: " + duration)
        context.system.shutdown
    }
  }
  def calculate(_tagSegMap: MHashMap[(Int, Int), Array[TagSegment]],
    toConfidence: (Int) => Double) = {
    /**
     * Attention: *MUST* clear here!!!
     */
    posToOpNode.clear()
    val tagSegMap = _tagSegMap.filter(x => toConfidence(x._2.size) > minConfidence)
    if (tagSegMap.size > 0) {
      val system = ActorSystem("DispatchTSCluster")
      val listener = system.actorOf(Props(new Listener), name = "listener")
      val master = system.actorOf(
        Props(new Master(tagSegMap, toConfidence, listener)),
        name = "master")
      master ! StartCalculation
      system.awaitTermination
    }
    posToOpNode
  }
}
