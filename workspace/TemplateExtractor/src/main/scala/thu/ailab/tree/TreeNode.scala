package thu.ailab.tree

class TreeNode(val name: String, val depth: Int) {
  /**
   * object equality methods
   */
  // Starts here
  override def equals(other: Any) = {
    other match {
      case that: TreeNode => 
        (that canEqual this) &&
        name == that.name &&
        depth == that.depth
      case _ => false
    }
  }
  override def hashCode = 41 * (41 + name.hashCode) + depth
  def canEqual(that: Any) = that.isInstanceOf[TreeNode]
  // Ends here
  
  override def toString = name + depth
}