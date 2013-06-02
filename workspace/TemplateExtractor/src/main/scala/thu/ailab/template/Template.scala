package thu.ailab.template

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{ HashMap => MHashMap }
import scala.collection.mutable.{ HashSet => MHashSet }
import scala.collection.JavaConversions._
import org.jsoup.nodes._
import org.jsoup.select.NodeVisitor
import thu.ailab.utils.Tools.withPrintWriter
import thu.ailab.sequence.TagSequence
import thu.ailab.distance.LCSWithPath
import thu.ailab.global.AppEntry
import thu.ailab.global.MyConfigFactory
import thu.ailab.tree.TreeBuilder
import thu.ailab.tree.VerboseTreeNode
import thu.ailab.tree.TreeNode
import thu.ailab.tree.HTMLSuffixTree

import ExType._
class Template(val tnArray: Array[TemplateNode]) {
  val essentialNodes = tnArray.filter(_.isEssential).map(_.asInstanceOf[EssentialNode])
  val eTreeNodes = essentialNodes.flatMap(_.getTreeNodes)
  val exPatternEss = eTreeNodes.zipWithIndex.filter(_._1.exType != MAGIC).map(x => 
    x._2 -> x._1.exType).toMap
  val essentialTagSeq = TagSequence.fromNodeArray(eTreeNodes, true)
  val optionalNodes = tnArray.filter(_.isOptional).map(_.asInstanceOf[OptionalNode])
  val posArray = essentialNodes.scanLeft(0)((acc, x) => acc + x.getTreeNodeCount)
  val posToOpNode = optionalNodes.zipWithIndex.map { x =>
    posArray(x._2) -> x._1
  }.toMap
  def makeEssentialTree() = {
    TPTreeNode.makeTPTree(essentialTagSeq.getCompact, posToOpNode)
  }
  def extract(thatTagSeq: TagSequence) = {
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
    val lcs = new LCSWithPath(essentialTagSeq, thatTagSeq)
    val ci = lcs.getCommonIndices.unzip
    val ciMap = lcs.getCommonIndices.toMap
    val exPattern = new MHashMap[Node, ExType]
    exPattern ++= exPatternEss.flatMap(x => 
      thatTagSeq.getCompact()(ciMap(x._1)).asInstanceOf[VerboseTreeNode].relatedRoots.map(_ -> x._2))
    val clcs = thatTagSeq.makeTagSequence(ci._2)
    assert(clcs.compactLength == essentialTagSeq.compactLength)
    val foo = new LCSWithPath(clcs, thatTagSeq)
    println("clcs length: " + clcs.compactLength)
    println(clcs)
    println("foo length: " + foo.getCommonIndices.length)    
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
    val jnodeArray = tpSeq.flatMap(_.getCompact).flatMap(_.asInstanceOf[VerboseTreeNode].relatedRoots)
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
  override def toString() = {
    tnArray.mkString("\n")
  }
  def toXML() = {
    <template>
      { tnArray.map(_.toXML) }
    </template>
  }
}

object Template {
  def fromXML(node: scala.xml.Node) = {
    new Template((node \ "templatenode" map (TemplateNode.fromXML(_))).toArray)
  }
}

object TestTemplate extends AppEntry {
  val templateFile = MyConfigFactory.getValue[String]("template.templateFile")
  val templatesXML = scala.xml.XML.loadFile(templateFile)
  val templateArray = (templatesXML \ "template" map (Template.fromXML(_))).toArray
  val tp = templateArray.last
  val fn = sys.props("user.home") + "/Data/blog/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_3d09ff700102dy0u.html"
  val vnodeArray = new TreeBuilder(fn).getVerboseTagSequence.toArray
  val thatTagSeq = TagSequence.fromNodeArray(vnodeArray, false)
  println(tp)
  tp.extract(thatTagSeq)
}
