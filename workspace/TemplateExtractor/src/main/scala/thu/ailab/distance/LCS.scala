package thu.ailab.distance

import scala.collection.mutable.ArrayBuffer
import thu.ailab.global._
import scala.annotation.tailrec

import thu.ailab.tree.TreeNode
import thu.ailab.tree.HTMLSuffixTree

object Direction extends Enumeration {
  type Direction = Value
  val DIAGONAL, DOWN, RIGHT = Value
}
import Direction._

abstract class Algorithm[T] {
  def getDistance(): Double
  def getCommonPath(): T
}
abstract class LCSArray(seq_1: Array[TreeNode], seq_2: Array[TreeNode])
extends Algorithm[Array[TreeNode]] with LoggerTrait {
  //val maxDepth = (seq1.maxBy(_.depth).depth + seq2.maxBy(_.depth).depth) / 2
  val maxDepth = math.max(seq_1.maxBy(_.depth).depth, seq_2.maxBy(_.depth).depth)
  val (seq1, seq2) = (seq_1.filter(_.depth <= maxDepth), seq_2.filter(_.depth <= maxDepth))
  val (len1, len2) = (seq1.length, seq2.length)
  protected def getCostFromDepth(depth: Int) = maxDepth - depth
  protected def sumCost(seq: Array[TreeNode]) = {
    seq.foldLeft(0)((acc, node) => acc + getCostFromDepth(node.depth))
  }
  protected def similarityToDistance(similarity: Int) = {
    1 - 1.0 * similarity / math.max(sumCost(seq1), sumCost(seq2))
  }
}

class LCSArrayWithPath(seq_1: Array[TreeNode], seq_2: Array[TreeNode]) 
extends LCSArray(seq_1, seq_2) {
  private def getLCSAndPath() = { 
    val table = Array.ofDim[Int](len1 + 1, len2 + 1)
    val directionMap = Array.ofDim[Direction](len1, len2)
    for (i <- 1 to seq1.length; j <- 1 to seq2.length) {
      if (seq1(i - 1) == seq2(j - 1)) {
        table(i)(j) = table(i - 1)(j - 1) + getCostFromDepth(seq1(i - 1).depth)
        directionMap(i - 1)(j - 1) = DIAGONAL
      } else if (table(i - 1)(j) > table(i)(j - 1)) {
        table(i)(j) = table(i - 1)(j)
        directionMap(i - 1)(j - 1) = DOWN
      } else {
        table(i)(j) = table(i)(j - 1)
        directionMap(i - 1)(j - 1) = RIGHT
      }
    }
    println("similarity: " + table(len1)(len2))
    (similarityToDistance(table(len1)(len2)), backtraceLCS(directionMap))
  }
  /**
   * Recursively find the path
   */
  private def backtraceLCS(directionMap: Array[Array[Direction]]): Array[TreeNode] = {
    @tailrec
    def helper(x: Int, y: Int, acc: List[TreeNode]): Array[TreeNode] = {
      if (x > 0 && y > 0) {
        directionMap(x)(y) match {
          case DOWN => helper(x, y - 1, acc)
          case RIGHT => helper(x - 1, y, acc)
          case DIAGONAL => {
            println(seq1(x))
            helper(x - 1, y - 1, seq1(x) :: acc)
          }
        }
      } else {
        (seq1(x) :: acc).toArray
      }        
    }
    helper(directionMap.length - 1, directionMap(0).length - 1, Nil)
  }
  val (distance, commonPath) = getLCSAndPath()
  def getDistance() = distance
  def getCommonPath() = commonPath
}

class LCSArraySpaceOptimized(seq_1: Array[TreeNode], seq_2: Array[TreeNode])
extends LCSArray(seq_1, seq_2) {
  case class NotImplementedException(msg: String) extends Exception
  private def getLCSSpaceOptimized() = {
    val twoRows = Array.ofDim[Int](2, len2 + 1) // For simplicity, always use the second one
    for {
      i <- 1 to len1
      nextRow = i & 1
      currentRow = 1 - nextRow
      j <- 1 to len2
    } {
      if (seq1(i - 1) == seq2(j - 1))
        twoRows(nextRow)(j) = twoRows(currentRow)(j - 1) + getCostFromDepth(seq1(i - 1).depth)
      else
        twoRows(nextRow)(j) = math.max(twoRows(nextRow)(j - 1), twoRows(currentRow)(j))
    }
    twoRows(len1 % 2)(len2)
  }
  val distance = similarityToDistance(getLCSSpaceOptimized())
  def getDistance() = distance
  def getCommonPath() = throw new NotImplementedException("This method has not been implemented")
}

object TestLCS extends AppEntry {
  import thu.ailab.tree.TreeBuilder
  import thu.ailab.utils.Tools.timeIt
  val fnPrefix = System.getProperty("user.home") + "/Programs/BachelorThesis/Data/blog1000/"
  val fn1 = fnPrefix + "http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00f2e45101017icv.html"
  val fn2 = fnPrefix + "http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00f2e45101017icz.html"
  val tagSeq1 = HTMLSuffixTree.stripDuplicates(new TreeBuilder(fn1).getTagSequence.toArray)
  val tagSeq2 = HTMLSuffixTree.stripDuplicates(new TreeBuilder(fn2).getTagSequence.toArray)
  val lcs = new LCSArrayWithPath(tagSeq1, tagSeq2)
  println(lcs.getDistance)
  println(tagSeq1.mkString(" "))
  println(tagSeq2.mkString(" "))
  println(lcs.getCommonPath mkString " ")
}

class LCSList[T](seq1: List[T], seq2: List[T]) 
extends Algorithm[List[T]] with LoggerTrait {
  val (len1, len2) = (seq1.length, seq2.length)
//    val (table, path) = getTableAndPathIter(seq1, seq2)
//    val similarity = table(len1)(len2)
//    val lcs = backtraceLCS(seq1, path)
//    val similarity = getLCSRecur(seq1, seq2).length
  val similarity = getLCSOptimized()
  val distance = 1 - 1.0 * similarity / math.max(len1, len2)
  val commonPath = backtraceLCS(getTableAndDirectionIter()._2)
  def getDistance() = distance
  def getCommonPath() = commonPath
  /**
   * Recursive version, very slow.
   * For reference only.
   */
  private def getLCSRecur(seq1: List[T], seq2: List[T]): List[T] = {
    def maxList(lst1: List[T], lst2: List[T]) = if (lst1.length > lst2.length) lst1 else lst2
    (seq1, seq2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (x1 :: xs1, x2 :: xs2) =>
        if (x1 == x2) x1::getLCSRecur(xs1, xs2)
        else maxList(getLCSRecur(seq1, xs2), getLCSRecur(xs1, seq2))
    }
  }
  /**
   * Iterative version. 
   * Use this in application.
   */
  private def getTableAndDirectionIter() = {
    val table = Array.ofDim[Int](len1 + 1, len2 + 1)
    val directionMap = Array.ofDim[Direction](len1, len2)
    for (i <- 1 to seq1.length; j <- 1 to seq2.length) {
      if (seq1(i - 1) == seq2(j - 1)) {
        table(i)(j) = table(i - 1)(j - 1) + 1
        directionMap(i - 1)(j - 1) = DIAGONAL
      } else if (table(i - 1)(j) > table(i)(j - 1)) {
        table(i)(j) = table(i - 1)(j)
        directionMap(i - 1)(j - 1) = DOWN
      } else {
        table(i)(j) = table(i)(j - 1)
        directionMap(i - 1)(j - 1) = RIGHT
      }
    }
    (table, directionMap)
  }
  /**
   * Recursively find the path
   */
  private def backtraceLCS(directionMap: Array[Array[Direction]]): List[T] = {
    def helper(x: Int, y: Int): List[T] = {
      if (x < 0 || y < 0)
        return Nil
      directionMap(x)(y) match {
        case DIAGONAL => seq1(x) :: helper(x - 1, y - 1)
        case DOWN => helper(x - 1, y)
        case RIGHT => helper(x, y - 1)
      }
    }
    helper(directionMap.length - 1, directionMap(0).length - 1).reverse
  }
  private def getLCSOptimized() = {
    val twoRows = Array.ofDim[Int](2, len2 + 1) // For simplicity, always use the second one
    for {
      i <- 1 to len1
      nextRow = i & 1
      currentRow = 1 - nextRow
      j <- 1 to len2
    } {
      if (seq1(i - 1) == seq2(j - 1))
        twoRows(nextRow)(j) = twoRows(currentRow)(j - 1) + 1
      else
        twoRows(nextRow)(j) = math.max(twoRows(nextRow)(j - 1), twoRows(currentRow)(j))    
    }
    twoRows(len1 % 2)(len2)
  }
}

