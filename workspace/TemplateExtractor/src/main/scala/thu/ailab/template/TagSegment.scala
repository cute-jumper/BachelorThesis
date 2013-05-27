package thu.ailab.template

import thu.ailab.document.TagSequence
import thu.ailab.tree.TreeNode

class TagSegment(val fileId: Int, prevIndices:(Int, Int), nextIndices: (Int, Int),
    tagSeq: TagSequence) {
  val prevCenterIndex = prevIndices._1
  val nextCenterIndex = nextIndices._1
  val beginIndex = prevIndices._2 + 1
  val endIndex = nextIndices._2
  val treeNodeStored = tagSeq.getSeparate.slice(beginIndex, endIndex)
  override def equals(other: Any) = {
    other match {
      case that: TagSegment => 
        (that canEqual this) && this.hashCode == that.hashCode
      case _ => false
    }
  }
  def canEqual(that: Any) = that.isInstanceOf[TagSegment]
  override val hashCode = 41 * treeNodeStored.foldLeft(1)((acc, x) =>
    41 * acc + x.hashCode) + (41 * prevCenterIndex + nextCenterIndex)
  
}