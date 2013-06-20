package thu.ailab.sequence

import scala.annotation.tailrec

import thu.ailab.global.MyConfigFactory
import thu.ailab.tree.TreeNode

/**
 * Used to stored the alignment information when building templates 
 */
class TagSegment(tagSeqInput: TagSequence, 
    val beginIndex: Int,
    val endIndex: Int,
    val fileId: Int) {
  val tagSeq = tagSeqInput.makeTagSequence(beginIndex + 1 until endIndex)
  def getTagSeq() = tagSeq
  override def equals(other: Any) = {
    other match {
      case that: TagSegment => 
        (that canEqual this) && this.hashCode == that.hashCode
      case _ => false
    }
  }
  def canEqual(that: Any) = that.isInstanceOf[TagSegment]
  override val hashCode = 
    41 * tagSeq.getCompact.foldLeft(1)((acc, x) => 41 * acc + x.hashCode) +
    (41 * beginIndex.hashCode + endIndex.hashCode)
  override def toString() = {
    tagSeq.getCompact.mkString(" ")
  }
}