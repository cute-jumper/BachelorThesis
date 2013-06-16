package thu.ailab.template

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{ HashMap => MHashMap, HashSet => MHashSet }
import scala.collection.JavaConversions._
import org.jsoup.nodes._
import org.jsoup.select.NodeVisitor
import thu.ailab.global._
import thu.ailab.sequence._
import thu.ailab.tree._
import thu.ailab.distance.{LCSArraySpaceOptimized, LCSWithPath}
import thu.ailab.utils.Tools.withPrintWriter
import ExType._
import thu.ailab.distance.LCSArraySpaceOptimized

class Template(val tnArray: Array[TemplateNode], val centerId: Int) {
  val eNodes = tnArray.filter(_.isEssential).map(_.asInstanceOf[EssentialNode])
  val eTagSeq = TagSequence.fromNodeArray(eNodes.flatMap(_.getTreeNodes), true)
  val exPatternEss = eTagSeq.getCompact.zipWithIndex.filter(_._1.exType != MAGIC).map(x =>
    x._2 -> x._1.exType).toMap
  val oNodes = tnArray.filter(_.isOptional).map(_.asInstanceOf[OptionalNode])
  val posArray = eNodes.scanLeft(0)((acc, x) => acc + x.getTreeNodeCount)
  val posToOpNode = oNodes.zipWithIndex.map { x =>
    posArray(x._2) -> x._1
  }.toMap
  def getETagSeqLength() = {
    eTagSeq.getCompact.length
  }
  def makeEssentialTree() = {
    TPTreeNode.makeTPTree(eTagSeq.getCompact, posToOpNode)
  }
  def extract(thatTagSeq: TagSequence) = {
    val (matchedNodes, exPattern) = getMatchedNodesAndExPattern(thatTagSeq)
    val jnodeArray = matchedNodes.getCompact.flatMap(_.asInstanceOf[VerboseTreeNode].relatedRoots)
    val nodePool = jnodeArray.toSet
    for (node <- jnodeArray) {
      println(node.nodeName + ": " + getNodeText(node, nodePool))
    }
    for ((node, exType) <- exPattern) {
      println("=" * 100)
      println(exType)
      println(getNodeText(node, nodePool))
    }
  }
  private val dataset = MyConfigFactory.getValue[String]("global.dataset")
  private val id2filename = scala.io.Source.fromFile(
          MyConfigFactory.getValue[String](dataset, "output.id2filename")).
          getLines.toArray
  private val centerTagSeq = new TagSeqFactory(id2filename).getInstance(centerId) 
  def distFromCenter(thatTagSeq: TagSequence) = {
    new LCSArraySpaceOptimized(centerTagSeq, thatTagSeq).getDistance
  }
  def getMatchedNodesAndExPattern(thatTagSeq: TagSequence) = {
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
    assert(clcs.compactLength == eTagSeq.compactLength)
    val tagSegMap = ClusterMethod.getTagSegMap(clcs,
      Some(1).iterator,
      (id: Int) => thatTagSeq)
    val tpSeq = new ArrayBuffer[TagSequence]
    var prevPos = 0
    for {
      range <- tagSegMap.keys.toSeq.sorted if posToOpNode.contains(range._1)
      ts = tagSegMap(range).head
    } {
      tpSeq += clcs.makeTagSequence(prevPos to range._1)
      val bundleArray = posToOpNode(range._1).bundleArray
      val retOption = findBestBundle(bundleArray, ts.getTagSeq)
      if (retOption.isDefined) {
        val ret = retOption.get
        tpSeq += ret._1
        exPattern ++= ret._2
      }
      prevPos = range._2
    }
    if (prevPos < clcs.compactLength) {
      tpSeq += clcs.makeTagSequence(prevPos until clcs.compactLength)
    }
    val tnArray = finalNormalize(tpSeq.flatMap(_.getCompact).toArray)
    val root = TPTreeNode.makeTPTree(tnArray, posToOpNode)
    println(root.toASCII())
    (TagSequence.fromNodeArray(tnArray, true), exPattern.toMap)
  }
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
    <template centerId={centerId.toString}>
      { tnArray.map(_.toXML) }
    </template>
  }
}

object Template {
  def fromXML(node: scala.xml.Node) = {
    new Template((node \ "templatenode" map (TemplateNode.fromXML(_))).toArray,
        node.attribute("centerId").get.text.toInt)
  }
}

