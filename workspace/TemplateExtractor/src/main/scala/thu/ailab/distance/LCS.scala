package thu.ailab.distance

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

import thu.ailab.global._
import thu.ailab.tree.TreeNode
import thu.ailab.tree.HTMLSuffixTree
import thu.ailab.sequence.TagSequence

object Direction extends Enumeration {
  type Direction = Value
  val DIAGONAL, DOWN, RIGHT = Value
}
import Direction._

abstract class Algorithm {
  def getDistance(): Double
  def getCommonIndices(): List[(Int, Int)]
}

abstract class LCS extends Algorithm {
  /**
   * Recursively find the path
   */
  protected def backtraceLCS(directionMap: Array[Array[Direction]]): List[(Int, Int)] = {
    @tailrec
    def helper(x: Int, y: Int, indices: List[(Int, Int)]): List[(Int, Int)] = {
      if (x > 0 && y > 0) {
        directionMap(x)(y) match {
          case DOWN => helper(x, y - 1, indices)
          case RIGHT => helper(x - 1, y, indices)
          case DIAGONAL => helper(x - 1, y - 1, (x, y) :: indices)
        }
      } else {
        (x, y) :: indices
      }
    }
    helper(directionMap.length - 1, directionMap(0).length - 1, Nil)
  }
}

abstract class LCSTagSequence(ts_1: TagSequence, ts_2: TagSequence)
extends LCS with LoggerTrait {
  val (seq1, seq2) = (ts_1.getCompact, ts_2.getCompact)
  val maxDepth = math.max(seq1.maxBy(_.depth).depth, seq2.maxBy(_.depth).depth)
  val (len1, len2) = (seq1.length, seq2.length)
  protected def getCostFromDepth(depth: Int) = maxDepth - depth + 1
  protected def sumCost(seq: Seq[Int]) = seq.map(getCostFromDepth).sum
  protected def similarityToDistance(similarity: Double) = {
    1 - 1.0 * similarity / math.max(sumCost(seq1.map(_.depth)),
        sumCost(seq2.map(_.depth)))
  }
}

class LCSWithPath(ts_1: TagSequence, ts_2: TagSequence)
extends LCSTagSequence(ts_1, ts_2) {
  def getLCSAndPath() = { 
    val table = Array.ofDim[Double](len1 + 1, len2 + 1)
    val directionMap = Array.ofDim[Direction](len1, len2)
    for ((e1, i)  <- seq1.zipWithIndex; (e2, j) <- seq2.zipWithIndex) {
      if (e1.shallowEquals(e2)) {
        table(i + 1)(j + 1) = table(i)(j) + getCostFromDepth(e1.depth)
        directionMap(i)(j) = DIAGONAL
      } else if (table(i)(j + 1) > table(i + 1)(j)) {
        table(i + 1)(j + 1) = table(i)(j + 1)
        directionMap(i)(j) = RIGHT
      } else {
        table(i + 1)(j + 1) = table(i + 1)(j)
        directionMap(i)(j) = DOWN
      }
    }
    (similarityToDistance(table(len1)(len2)), backtraceLCS(directionMap))
  }  
  val (distance, commonIndices) = getLCSAndPath()
  def getDistance() = distance
  def getCommonIndices() = commonIndices
}

class LCSArraySpaceOptimized(ts_1: TagSequence, ts_2: TagSequence)
extends LCSTagSequence(ts_1, ts_2) {
  case class NotImplementedException(msg: String) extends Exception
  private def getLCSSpaceOptimized() = {
    val twoRows = Array.ofDim[Int](2, len2 + 1) // For simplicity, always use the second one
    for {
      i <- 1 to len1
      nextRow = i & 1
      currentRow = 1 - nextRow
      j <- 1 to len2
    } {
      if (seq1(i - 1).shallowEquals(seq2(j - 1)))
        twoRows(nextRow)(j) = twoRows(currentRow)(j - 1) + getCostFromDepth(seq1(i - 1).depth)
      else
        twoRows(nextRow)(j) = math.max(twoRows(nextRow)(j - 1), twoRows(currentRow)(j))
    }
    twoRows(len1 % 2)(len2)
  }
  val distance = similarityToDistance(getLCSSpaceOptimized())
  def getDistance() = distance
  def getCommonIndices() = throw new NotImplementedException("This method has not been implemented")
}

object TestLCS extends App {
  import thu.ailab.tree.TreeBuilder
  import thu.ailab.utils.Tools.timeIt
  val fnPrefix = System.getProperty("user.home") + "/Programs/BachelorThesis/Data/blog1000/"
  val fn1 = fnPrefix + "http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_002b5d980100sf47.html"
  val fn2 = fnPrefix + "http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00f2e4510101714i.html"
  val tagSeq1 = new TagSequence(new TreeBuilder(fn1).getTagSequence.toArray, false)
  val tagSeq2 = new TagSequence(new TreeBuilder(fn2).getTagSequence.toArray, false)
  val lcs = new LCSWithPath(tagSeq1, tagSeq2)
  println(lcs.getDistance)
  println(lcs.getCommonIndices.length)
  println(tagSeq1.getCompact.mkString(" "))
  println(tagSeq2.getCompact.mkString(" "))
  def getAlignedString(indexPool: Set[Int], ts: Array[TreeNode]) = {
    for (i <- ts.indices) yield {
      if (indexPool.contains(i)) {
        ts(i).toString
      }
      else {
        (" " * (ts(i).toString.length))
      }
    }
  }
  val indexPool = lcs.getCommonIndices.unzip._1.toSet
  println(getAlignedString(indexPool, tagSeq1.getCompact) mkString " ")
}
