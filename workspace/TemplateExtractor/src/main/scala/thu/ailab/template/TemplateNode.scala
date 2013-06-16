package thu.ailab.template

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{ HashMap => MHashMap }

import thu.ailab.sequence.TagSequence
import thu.ailab.global.MyConfigFactory
import thu.ailab.distance.LCSWithPath

class TagSeqBundle(val tagSeq: TagSequence, val confidence: Double) {
  override def toString() = {
    tagSeq + " --> " + confidence
  }
  def getBundleWeightedLength() = {
    tagSeq.compactLength * confidence
  }
  def toXML() = {
    <tagseqbundle>
    {tagSeq.toXML}
    <confidence>{confidence}</confidence>
    </tagseqbundle>
  }
}

object TagSeqBundle {
  def fromXML(node: scala.xml.Node) = {
    val tagSeq =  TagSequence.fromXML((node \ "tagsequence").head)
    val confidence = (node \ "confidence").head.text.toDouble
    new TagSeqBundle(tagSeq, confidence)        
  }
}

object NodeType extends Enumeration {
  type NodeType = Value
  val ESSENTIAL, OPTIONAL = Value
}
import NodeType._

abstract class TemplateNode {
  val isOptional: Boolean
  val isEssential: Boolean = !isOptional
  val nodeType: NodeType
  def toXML(): scala.xml.Elem
}

class EssentialNode(val tagSeq: TagSequence) extends {
  override val isOptional = false
} with TemplateNode {
  override val nodeType = ESSENTIAL
  def getTreeNodes() = {
    tagSeq.getCompact
  }
  def getTreeNodeCount() = tagSeq.compactLength    
  override def toString() = {
    "E: " + tagSeq.toString
  }
  def toXML() = {
    <templatenode type={nodeType.toString}>{tagSeq.toXML}</templatenode>
  }  
}

class OptionalNode(val bundleArray: Array[TagSeqBundle]) extends {
  override val isOptional = true
} with TemplateNode {
  override val nodeType = OPTIONAL
  def getBundleCount() = bundleArray.size
  override def toString() = {
    "O: " + bundleArray.mkString(" | ")
  }
  def getAverageLength() = {
    val sum = bundleArray.foldLeft(0.0)((acc, x) => acc + x.getBundleWeightedLength)
    sum
  }
  def toXML() = {
    <templatenode type={nodeType.toString}>{bundleArray.map(_.toXML)}</templatenode>
  }  
}

case class UnknownNodeTypeException(msg: String) extends Exception

object TemplateNode {
  def fromXML(node: scala.xml.Node) = {
    withName(node.attribute("type").get.text) match {
      case ESSENTIAL => EssentialNode.fromXML(node)
      case OPTIONAL => OptionalNode.fromXML(node)
      case _ => throw UnknownNodeTypeException("UNKNOWN NODE TYPE!")
    }    
  }  
}

object EssentialNode {
  def fromXML(node: scala.xml.Node) = {
    new EssentialNode(TagSequence.fromXML((node \ "tagsequence").head))
  }
}

object OptionalNode {
  def fromXML(node: scala.xml.Node) = {
    new OptionalNode((node \ "tagseqbundle" map (TagSeqBundle.fromXML(_))).toArray)
  }
}
