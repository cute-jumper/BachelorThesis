package thu.ailab.distance

import scala.collection.mutable.ArrayBuffer
import thu.ailab.global._

abstract class Algorithm[T] {
  def run(p1: T, p2: T): Double
}

class LCSList[T] extends Algorithm[List[T]] with LoggerTrait {
  object Direction extends Enumeration {
    type Direction = Value
    val DIAGONAL, DOWN, RIGHT = Value
  }
  import Direction._
  def run(seq1: List[T], seq2: List[T]) = {
    val (len1, len2) = (seq1.length, seq2.length)
//    val (table, path) = getTableAndPathIter(seq1, seq2)
//    val similarity = table(len1)(len2)
//    val lcs = backtraceLCS(seq1, path)
//    val similarity = getLCSRecur(seq1, seq2).length
    val similarity = getLCSOptimized(seq1, seq2)
    val distance = 1 - 1.0 * similarity / math.max(len1, len2)
    distance
  }
  /**
   * Recursive version, very slow.
   * For reference only.
   */
  def getLCSRecur(seq1: List[T], seq2: List[T]): List[T] = {
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
  def getTableAndPathIter(seq1: List[T], seq2: List[T]) = {
    val (len1, len2) = (seq1.length, seq2.length)
    val table = Array.ofDim[Int](len1 + 1, len2 + 1)
    val path = Array.ofDim[Direction](len1, len2)
    for (i <- 1 to seq1.length; j <- 1 to seq2.length) {
      if (seq1(i - 1) == seq2(j - 1)) {
        table(i)(j) = table(i - 1)(j - 1) + 1
        path(i - 1)(j - 1) = DIAGONAL
      } else if (table(i - 1)(j) > table(i)(j - 1)) {
        table(i)(j) = table(i - 1)(j)
        path(i - 1)(j - 1) = DOWN
      } else {
        table(i)(j) = table(i)(j - 1)
        path(i - 1)(j - 1) = RIGHT
      }
    }
    (table, path)
  }
  /**
   * Recursively find the path
   */
  def backtraceLCS(seq: List[T], path: Array[Array[Direction]]): List[T] = {
    def helper(x: Int, y: Int): List[T] = {
      if (x < 0 || y < 0)
        return Nil
      path(x)(y) match {
        case DIAGONAL => seq(x) :: helper(x - 1, y - 1)
        case DOWN => helper(x - 1, y)
        case RIGHT => helper(x, y - 1)
      }
    }
    helper(path.length - 1, path(0).length - 1).reverse
  }
  def getLCSOptimized(seq1: List[T], seq2: List[T]) = {
    val (len1, len2) = (seq1.length, seq2.length)    
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

class LCSArray[T] extends Algorithm[Array[T]] with LoggerTrait {
  var 
  def run(seq1: Array[T], seq2: Array[T]) = {
    val (len1, len2) = (seq1.length, seq2.length)
    val similarity = getLCSOptimized(seq1, seq2)
    logger.info("Similarity: %d".format(similarity))
    val distance = 1 - 1.0 * similarity / math.max(len1, len2)
    distance    
  }
  def getLCSOptimized(seq1: Array[T], seq2: Array[T]) = {
    val (len1, len2) = (seq1.length, seq2.length)    
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

import thu.ailab.tree.TreeNode
class LCSArrayWithWeight extends Algorithm[Array[TreeNode]] with LoggerTrait {
  def run(seq1: Array[TreeNode], seq2: Array[TreeNode]) = {
    val (len1, len2) = (seq1.length, seq2.length)
    val similarity = getLCSOptimized(seq1, seq2)
    logger.info("Similarity: %d".format(similarity))
    val distance = 1 - 1.0 * similarity / math.max(len1, len2)
    distance    
  }
  def getLCSOptimized(seq1: Array[TreeNode], seq2: Array[TreeNode]) = {
    val (len1, len2) = (seq1.length, seq2.length)    
    val twoRows = Array.ofDim[Double](2, len2 + 1) // For simplicity, always use the second one
    for {
      i <- 1 to len1
      nextRow = i & 1
      currentRow = 1 - nextRow
      j <- 1 to len2
    } {
      if (seq1(i - 1) == seq2(j - 1))
        twoRows(nextRow)(j) = twoRows(currentRow)(j - 1) + 1 / seq1(i - 1).depth
      else
        twoRows(nextRow)(j) = math.max(twoRows(nextRow)(j - 1), twoRows(currentRow)(j))    
    }
    twoRows(len1 % 2)(len2)
  }  
}


object TestLCS extends AppEntry {
  val lcsAlgo = new LCSList[Char]
  val test = lcsAlgo.run("abcde...fg".toList, "asdfasbdfasdfcsdfdsdfefdasfsdfg".toList)
  println(test)
}