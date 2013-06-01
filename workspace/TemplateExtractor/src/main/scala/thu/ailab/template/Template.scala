package thu.ailab.template

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{ HashMap => MHashMap }
import thu.ailab.sequence.TagSequence

class Template(val tnArray: Array[TemplateNode]) {
  println(toString)
  makeEssentialTree
  def makeEssentialTree() = {
    val essentialNodes = tnArray.filter(_.isEssential).map(_.asInstanceOf[EssentialNode])
    val optionalNodes = tnArray.filter(_.isOptional).map(_.asInstanceOf[OptionalNode])
    val posArray = essentialNodes.scanLeft(-1)((acc, x) => acc + x.getTreeNodeCount)
    val posToOpNode = optionalNodes.zipWithIndex.map { x =>
      posArray(x._2) -> x._1
    }.toMap
    val root = TPTreeNode.makeTPTree(essentialNodes.flatMap(_.getTreeNodes), posToOpNode)
    println(root.toASCII(""))
  }  
  override def toString() = {
    tnArray.mkString("\n")
  }
  def toXML() = {
    <template>
    {tnArray.map(_.toXML)}
    </template>
  }
}

object Template {
  def fromXML(node: scala.xml.Node) = {
    new Template((node \ "templatenode" map (TemplateNode.fromXML(_))).toArray) 
  }
}
