package thu.ailab.sequence

import thu.ailab.tree.TreeNode

class Shingle(val treeNodeSeq: Seq[TreeNode],
  val nextShingle: Option[Shingle] = None) {
  val mainHashCode = 
    treeNodeSeq.foldLeft(1)((acc, node) => 41 * acc + node.hashCode)  
  def getMain() = treeNodeSeq
  def getNext() = nextShingle
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
  override def hashCode() = {
    41 * mainHashCode + nextShingle.hashCode
  }
  def canEqual(that: Any) = that.isInstanceOf[Shingle]
  override def toString() = {
    def getString(shingleOption: Option[Shingle]) = {
      shingleOption match {
        case Some(s) => s.getMain.mkString(" ")
        case x => x.toString
      }
    }
    (treeNodeSeq mkString " ")
    //+ " --> " + ("before: " + getString(prevShingle)) + (" after: " + getString(nextShingle))
  }
}
