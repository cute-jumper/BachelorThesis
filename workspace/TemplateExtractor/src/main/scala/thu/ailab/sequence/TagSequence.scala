package thu.ailab.sequence

import scala.collection.mutable.{HashMap => MHashMap}
import thu.ailab.tree.TreeNode
import thu.ailab.distance.LCSWithPath
import thu.ailab.tree.HTMLSuffixTree
import thu.ailab.tree.VerboseTreeNode
import thu.ailab.tree.TreeBuilder

/**
 * Class storing information the sequence of TreeNode after
 * all the duplicated have been stripped.
 * 
 * Constructor is private. Use constructor functions in the
 * companion object to construct the instances.
 */
class TagSequence private (compactArray: Array[TreeNode]) {
  val separateArray = compactArray flatMap (_.getSeparateNodes)
  def getCompact() = compactArray
  val compactLength = compactArray.length
  /**
   * Make TagSequence out of a sequence of indices
   */
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
   * Useless. Make all private. 
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
  /**
   * Used for content extracting system
   */
  def fromNodeArray(_inputArray: Array[VerboseTreeNode], isCompact: Boolean) = {
    val inputArray = 
      if (isCompact) _inputArray 
      else HTMLSuffixTree.stripDuplicates(_inputArray)
    new TagSequence(inputArray.map(_.asInstanceOf[TreeNode]))
  }
  /**
   * Used for template building system
   */
  def fromNodeArray(_inputArray: Array[TreeNode], isCompact: Boolean) = {
    val inputArray = 
      if (isCompact) _inputArray 
      else HTMLSuffixTree.stripDuplicates(_inputArray)
    new TagSequence(inputArray)
  }
  /**
   * Only use this when doing pre-processing, 
   * because it will introduce extra overhead.
   */
  def fromNodeArrayForPrep(_inputArray: Array[TreeNode], isCompact: Boolean) = {
    val inputArray = 
      if (isCompact) _inputArray 
      else HTMLSuffixTree.stripDuplicates(_inputArray, true)
    new TagSequence(inputArray)
  }
  def fromFile(name: String) = {
    TagSequence.fromNodeArray(new TreeBuilder(name).getTagSequence.toArray, false) 
  }
  def fromXML(node: scala.xml.Node) = {
    TagSequence.fromNodeArray((node \ "treenode" map (TreeNode.fromXML(_))).toArray, true)
  }
}