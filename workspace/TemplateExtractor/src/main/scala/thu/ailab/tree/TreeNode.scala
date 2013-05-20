package thu.ailab.tree

class TreeNode(val nameArray: Array[String],
    val depthArray: Array[Int], 
    val allowMultiple: Boolean) {
  def this(name: String, depth: Int, allowMultiple: Boolean = false) = {
    this(Array[String](name), Array[Int](depth), allowMultiple)
  }
  def this(treeNode: TreeNode, allowMultiple: Boolean) = {
    this(treeNode.nameArray, treeNode.depthArray, allowMultiple)
  }
  private val hashCodePairs = nameArray zip depthArray map { x =>
    41 * (41 + x._1.hashCode()) + x._2.hashCode()
  }
  private val isDuplicate = !hashCodePairs.exists(_ != hashCodePairs(0))
  val name = if (isDuplicate) nameArray(0) else nameArray.mkString
  val minDepth = depthArray(0)
  val maxDepth = depthArray(depthArray.length - 1)
  val depth = minDepth
  /**
   * object equality methods
   */
  // Starts here
  override def equals(other: Any) = {
    other match {
      case that: TreeNode => 
        (that canEqual this) && this.hashCode == that.hashCode
//        name == that.name &&
//        depth == that.depth
      case _ => false
    }
  }
  override val hashCode = {
    if (isDuplicate)
      hashCodePairs(0)
    else
      hashCodePairs.foldLeft(1)((acc, x) => 41 * acc + x)
  }
  def canEqual(that: Any) = that.isInstanceOf[TreeNode]
  // Ends here
  
  override val toString = {
    if (isDuplicate)
      name + depth
    else
      nameArray.zip(depthArray).map(x => x._1 + x._2).mkString
  }
}

object TreeNode {
  def merge(treeNodeArray: Array[TreeNode]) = {
    val startValue = (Array[String](), Array[Int]())
    val paramPair = treeNodeArray.foldLeft(startValue){(acc, x) => 
      (acc._1 ++ x.nameArray, acc._2 ++ x.depthArray)
    }
    new TreeNode(paramPair._1, paramPair._2, true)
  }
}