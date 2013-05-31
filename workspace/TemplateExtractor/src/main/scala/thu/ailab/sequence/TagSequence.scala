package thu.ailab.sequence

import scala.collection.mutable.{HashMap => MHashMap}
import thu.ailab.tree.TreeNode
import thu.ailab.distance.LCSWithPath
import thu.ailab.tree.HTMLSuffixTree

class TagSequence(inputArray: Array[TreeNode], isCompact: Boolean) {
  val (compactArray, separateArray) = if (isCompact) {
    (inputArray, inputArray flatMap (_.getSeparateNodes))
  } else {
    (HTMLSuffixTree.stripDuplicates(inputArray), inputArray)
  }
  def getCompact() = compactArray
  val compactLength = compactArray.length
  def makeTagSequence(indices: Seq[Int]) = {
    new TagSequence(indices.map(compactArray(_)).toArray, true)
  }
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
    new TagSequence(counts.zipWithIndex filter { x =>
      compactArray(x._2).innerSize == x._1
    } map { x =>
      compactArray(x._2)
    }, true)
  }
  private def getCompactFromSeparate(sepIndices: Seq[Int]) = {
    sepIndices.map(x => compactArray(sepToCom(x)))
  }
}
