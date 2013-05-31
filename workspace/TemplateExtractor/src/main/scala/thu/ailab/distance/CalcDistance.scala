package thu.ailab.distance

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.concurrent.duration._
import akka.event.Logging
import thu.ailab.sequence.{TagSequence, TagSeqFactory}
import thu.ailab.utils.Point
import thu.ailab.distance._
import thu.ailab.tree.TreeNode
import thu.ailab.global._
import thu.ailab.utils.Tools.timeIt

abstract class CalcDistance extends LoggerTrait {
  /**
   * Messages
   */
  sealed trait Message
  case object StartCalculation extends Message
  case class AreaSplit(p1: Point, p2: Point) extends Message
  case class AreaFinished() extends Message
  case class AllFinished(duration: Duration) extends Message  
  /**
   * Abstract fields and methods
   */
  val totalSize: Int
  def getSequence(id: Int): TagSequence
  def postCalculation(): Unit  
  /**
   * Constructor body
   */
  val distArray = new Array[Double](totalSize * (totalSize - 1) / 2)
  
  def doCalculation() = {
    calculate(nrOfWorkers = MyConfigFactory.getValue[Int]("actor.nrOfWorkers"), 
        pieceLength = MyConfigFactory.getValue[Int]("actor.pieceLength"))
    distArray
  }

  def calcDistance(id1: Int, id2: Int) = {
    new LCSArraySpaceOptimized(getSequence(id1), getSequence(id2)).getDistance()
  }
  class Worker extends Actor {
    def receive = {
      case AreaSplit(p1, p2) =>
        val (count, duration) = timeIt(calculateArea(p1, p2))
        //logger.info("Time spent: %f at %d".format(duration, count))
        sender ! AreaFinished
    }
    def calculateArea(p1: Point, p2: Point) = {
      var acc = 0 
      for (i <- p1.x + 1 to p2.x; j <- p1.y until math.min(p2.y, i)) {
        distArray((i - 1) * i / 2 + j) = calcDistance(i, j)
        acc += 1
      }
      acc
    }
  }
  class Master(nrOfWorkers: Int,
      pieceLength: Int,
      listener: ActorRef) extends Actor {
    val start = System.currentTimeMillis()
    val workerRouter = context.actorOf(
      Props(new Worker).withRouter(RoundRobinRouter(nrOfWorkers)), 
      name = "workRouter")
    val nrOfHSplit = (totalSize - 2) / pieceLength + 1
    val nrOfMessages = (nrOfHSplit + 1) * nrOfHSplit / 2
    //logger.info("Total area count: %d".format(nrOfMessages))
    var finishedCount = 0
    def receive = {
      case StartCalculation =>
        var acc = 1
        for (i <- 0 until nrOfHSplit; j <- 0 to i) {
          //logger.info("Start %d worker".format(acc))
          acc += 1
          workerRouter ! AreaSplit(Point(i * pieceLength, j * pieceLength), 
              Point(math.min((i + 1) * pieceLength, totalSize - 1),
                  math.min((j + 1) * pieceLength, totalSize - 1)))
        }
      case AreaFinished => 
        finishedCount += 1
        //logger.info("FinishedCount: %d".format(finishedCount))
        if (finishedCount == nrOfMessages) {
          listener ! AllFinished((System.currentTimeMillis() - start).millis)
          context.stop(self)
        }
    }
  }
  
  class Listener extends Actor {
    def receive = {
      case AllFinished(duration) =>
        logger.info("Calculation time: %s".format(duration))
        postCalculation()
        context.system.shutdown
    }
  }
     
  private def calculate(nrOfWorkers: Int, pieceLength: Int) = {
    val system = ActorSystem("CalcSimilarities")
    val listener = system.actorOf(Props(new Listener), name = "listener")
    val master = system.actorOf(Props(new Master(
        nrOfWorkers, pieceLength, listener)),
        name = "master")
    master ! StartCalculation
    system.awaitTermination
  }
  
}