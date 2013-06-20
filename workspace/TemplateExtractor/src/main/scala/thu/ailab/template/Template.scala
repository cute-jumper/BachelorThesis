package thu.ailab.template

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{ HashMap => MHashMap, HashSet => MHashSet }
import scala.collection.JavaConversions._
import org.jsoup.nodes._
import org.jsoup.select.NodeVisitor
import thu.ailab.global._
import thu.ailab.sequence._
import thu.ailab.tree._
import thu.ailab.distance.{ LCSArraySpaceOptimized, LCSWithPath }
import thu.ailab.utils.Tools.withPrintWriter
import ExType._
import thu.ailab.distance.LCSArraySpaceOptimized

class Template(val tnArray: Array[TemplateNode], val centerFile: String) extends LoggerTrait {
  val eNodes = tnArray.filter(_.isEssential).map(_.asInstanceOf[EssentialNode])
  val eTagSeq = TagSequence.fromNodeArray(eNodes.flatMap(_.getTreeNodes), true)
  val exPatternEss = eTagSeq.getCompact.zipWithIndex.filter(_._1.exType != MAGIC).map(x =>
    x._2 -> x._1.exType).toMap
  val oNodes = tnArray.filter(_.isOptional).map(_.asInstanceOf[OptionalNode])
  val posArray = eNodes.scanLeft(0)((acc, x) => acc + x.getTreeNodeCount)
  val posToOpNode = oNodes.zipWithIndex.map { x =>
    posArray(x._2) -> x._1
  }.toMap
  /**
   * Make the essential node tree
   */
  def makeEssentialTree() = {
    TPTreeNode.makeTPTree(eTagSeq.getCompact, posToOpNode)
  }
  /**
   * Extract the information.
   * Return field name and corresponding content.
   */
  def extract(thatTagSeq: TagSequence) = {
    val (matchedNodes, exPattern) = getMatchedNodesAndExPattern(thatTagSeq)
    val jnodeArray = matchedNodes.getCompact.flatMap(_.asInstanceOf[VerboseTreeNode].relatedRoots)
    val jnodeMap = matchedNodes.getCompact.flatMap { x =>
      val vtn = x.asInstanceOf[VerboseTreeNode]
      vtn.relatedRoots.map(_ -> vtn)
    }.toMap
    val nodePool = jnodeArray.toSet
    for ((node, exType) <- exPattern) yield {
      (exType, getNodeText(node, nodePool))
    }
  }
  private lazy val dataset = MyConfigFactory.getValue[String]("global.dataset")
  private lazy val centerTagSeq = TagSequence.fromFile(centerFile)
  def distFromCenter(thatTagSeq: TagSequence) = {
    new LCSArraySpaceOptimized(centerTagSeq, thatTagSeq).getDistance
  }
  /**
   * This function is responsible for finding matched nodes and
   * extraction patterns.
   */
  def getMatchedNodesAndExPattern(thatTagSeq: TagSequence) = {
    /**
     * normalize the tree
     */
    def finalNormalize(tnArray: Array[TreeNode]): Array[TreeNode] = {
      var prev = tnArray.head
      for ((tn, idx) <- tnArray.tail.zipWithIndex) {
        if (tn.depth - prev.depth > 1) {
          tnArray(idx) = null
        } else {
          prev = tn
        }
      }
      tnArray.filter(_ != null)
    }
    /**
     * find best matched bundle in optional node
     */
    def findBestBundle(bundleArray: Array[TagSeqBundle],
      tagSeq: TagSequence): Option[(TagSequence, Map[Node, ExType])] = {
      for (bundle <- bundleArray) {
        val lcs = new LCSWithPath(bundle.tagSeq, tagSeq)
        val ci = lcs.getCommonIndices.unzip
        val ciMap = lcs.getCommonIndices.toMap
        if (ciMap.size == bundle.tagSeq.compactLength) {
          val exPatternOpt = bundle.tagSeq.getCompact.zipWithIndex.filter(_._1.exType != MAGIC).flatMap { x =>
            tagSeq.getCompact()(ciMap(x._2)).asInstanceOf[VerboseTreeNode].relatedRoots.map(_ -> x._1.exType)
          }.toMap
          return Some(tagSeq.makeTagSequence(ci._2), exPatternOpt)
        }
      }
      None
    }
    val lcs = new LCSWithPath(eTagSeq, thatTagSeq)
    val ci = lcs.getCommonIndices.unzip
    val ciMap = lcs.getCommonIndices.toMap
    val exPattern = new MHashMap[Node, ExType]
    exPattern ++= exPatternEss.flatMap(x =>
      thatTagSeq.getCompact()(ciMap(x._1)).asInstanceOf[VerboseTreeNode].relatedRoots.map(_ -> x._2))
    val clcs = thatTagSeq.makeTagSequence(ci._2)
    if (clcs.compactLength != eTagSeq.compactLength) {
      /** ATTENTION: This is *NOT* the right way to deal with this condition.
       * 
       * Here, for simplicity, we only log this situation. 
       * However, maybe we should add a handle function
       * to handle this situation in the future. 
       */
      logger.info("-" * 10 + "Potential Error Begins" + "-" * 10)
      logger.info(clcs.toString)
      logger.info(eTagSeq.toString)
      logger.info("-" * 10 + "Potential Error Ends" + "-" * 10)
      //assert(false), brute-force way
    }
    val tagSegMap = ClusterMethod.getTagSegMap(clcs,
      Some(1).iterator,//tricky
      (id: Int) => thatTagSeq)
    val tpSeq = new ArrayBuffer[TagSequence]
    var prevPos = 0
    for {
      range <- tagSegMap.keys.toSeq.sorted if posToOpNode.contains(range._1)
      ts = tagSegMap(range).head
    } {
      tpSeq += clcs.makeTagSequence(prevPos to range._1)
      val bundleArray = posToOpNode(range._1).bundleArray
      findBestBundle(bundleArray, ts.getTagSeq) match {
        case Some((bestTagSeq, bestExPattern)) =>
          tpSeq += bestTagSeq
          exPattern ++= bestExPattern
        case _ =>
      }
      prevPos = range._2
    }
    if (prevPos < clcs.compactLength) {
      tpSeq += clcs.makeTagSequence(prevPos until clcs.compactLength)
    }
    val tnArray = finalNormalize(tpSeq.flatMap(_.getCompact).toArray)
    val root = TPTreeNode.makeTPTree(tnArray, posToOpNode)
    (TagSequence.fromNodeArray(tnArray, isCompact=true), exPattern.toMap)
  }
  /**
   * Get node text which doesn't belong to any other nodes in the templates
   */
  def getNodeText(node: Node, nodePool: Set[Node]) = {
    val nodeText = new StringBuilder
    for (child <- node.childNodes if !nodePool.contains(child)) {
      val text = child match {
        case n: Element => n.text
        case n: TextNode => n.getWholeText()
        case n: DataNode => n.getWholeData()
        case n => n.toString
      }
      nodeText ++= text.trim
    }
    nodeText.toString
  }
  override def toString() = {
    tnArray.mkString("\n")
  }
  def toXML() = {
    <template centerFile={ centerFile }>
      { tnArray.map(_.toXML) }
    </template>
  }
}

object Template {
  def fromXML(node: scala.xml.Node) = {
    new Template((node \ "templatenode" map (TemplateNode.fromXML(_))).toArray,
      node.attribute("centerFile").get.text)
  }
  /**
   * This function is used when converting the old format
   * XMLs to new format XMLs.
   */
  def fromOldXML(node: scala.xml.Node) = {
    val tnArray = (node \ "templatenode" map (TemplateNode.fromXML(_))).toArray
    val centerId = node.attribute("centerId").get.text.toInt
    val dataset = MyConfigFactory.getValue[String]("global.dataset")
    val id2filename = scala.io.Source.fromFile(MyConfigFactory.getValue[String](dataset, "output.id2filename")).getLines.toArray
    new Template(tnArray, id2filename(centerId))
  }
}

