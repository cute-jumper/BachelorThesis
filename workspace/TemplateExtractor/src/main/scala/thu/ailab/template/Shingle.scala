package thu.ailab.template

import thu.ailab.tree.TreeNode

class Shingle(val treeNodeArray: Array[TreeNode], val father: TreeNode) {
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
    father.hashCode
  }
  def canEqual(that: Any) = that.isInstanceOf[Shingle]
  override def toString() = {
    (treeNodeArray mkString " ") + "--> father: " + father
  }
}
