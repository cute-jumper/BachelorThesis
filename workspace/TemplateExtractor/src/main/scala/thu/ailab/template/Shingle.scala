package thu.ailab.template

import thu.ailab.tree.TreeNode

class Shingle(val treeNodeArray: Seq[TreeNode], 
    val before: Option[TreeNode], 
    val after: Option[TreeNode]) {
  def getNeighbor() = (before, after)
  def getMain() = treeNodeArray
  /**
   * object equality methods: equals, canEqual, hashCode
   */
  override def equals(other: Any) = {
    other match {
      case that: Shingle =>
        (that canEqual this) && this.hashCode == that.hashCode
      case _ => false
    }
  }
  override val hashCode = {
    41 * treeNodeArray.foldLeft(1)((acc, node) => 41 * acc + node.hashCode) + 
    (41 * before.hashCode + after.hashCode)
  }
  def canEqual(that: Any) = that.isInstanceOf[Shingle]
  override def toString() = {
    (treeNodeArray mkString " ") + " --> " + ("before: " + before) + (" after: ") + after
  }
}
