package thu.ailab.sequence

import scala.collection.mutable.{HashMap => MHashMap}
import thu.ailab.tree.TreeNode
import thu.ailab.distance.LCSWithPath
import thu.ailab.tree.HTMLSuffixTree
import thu.ailab.tree.VerboseTreeNode

class TagSequence private (compactArray: Array[TreeNode]) {
  val separateArray = compactArray flatMap (_.getSeparateNodes)
  def getCompact() = compactArray
  val compactLength = compactArray.length
  def makeTagSequence(indices: Seq[Int]) = {
    TagSequence.fromNodeArray(indices.map(compactArray(_)).toArray, true)
  }
  override def toString() = {
    compactArray.mkString(" ")
  }
  def toXML() = {
    <tagsequence>
    {compactArray.map(_.toXML)}
    </tagsequence>
  }
  /**
   * Useless
   */
  private def getSeparate() = separateArray
  private val separateLength = separateArray.length
  private val sepToCom = {
    val ret = new Array[Int](separateArray.size)
    var start = 0
    for ((i, idx) <- compactArray.zipWithIndex) {
      val end = start + i.innerSize
      start until end foreach {
        ret(_) = idx
      }
      start = end
    }
    ret
  }
  private def getNormalizeLCS(sepIndices: Seq[Int]) = {
    val counts = new Array[Int](compactArray.size)
    for (i <- sepIndices) {
      counts(sepToCom(i)) += 1
    }
    TagSequence.fromNodeArray(counts.zipWithIndex filter { x =>
      compactArray(x._2).innerSize == x._1
    } map { x =>
      compactArray(x._2)
    }, true)
  }
  private def getCompactFromSeparate(sepIndices: Seq[Int]) = {
    sepIndices.map(x => compactArray(sepToCom(x)))
  }
}

object TagSequence {
  def fromNodeArray(_inputArray: Array[VerboseTreeNode], isCompact: Boolean) = {
    val inputArray = 
      if (isCompact) _inputArray 
      else HTMLSuffixTree.stripDuplicates(_inputArray)
    new TagSequence(inputArray.map(_.asInstanceOf[TreeNode]))
  }
  def fromNodeArray(_inputArray: Array[TreeNode], isCompact: Boolean) = {
    val inputArray = 
      if (isCompact) _inputArray 
      else HTMLSuffixTree.stripDuplicates(_inputArray)
    new TagSequence(inputArray)
  }
  def fromXML(node: scala.xml.Node) = {
    TagSequence.fromNodeArray((node \ "treenode" map (TreeNode.fromXML(_))).toArray, true)
  }  
}