package thu.ailab.sequence

import scala.annotation.tailrec

import thu.ailab.global.MyConfigFactory
import thu.ailab.tree.TreeNode

class TagSegment(tagSeqInput: TagSequence, 
    val beginIndex: Int,
    val endIndex: Int,
    val fileId: Int) {
  val tagSeq = tagSeqInput.makeTagSequence(beginIndex + 1 until endIndex)
  def getTagSeq() = tagSeq
  lazy val shingles = {
    val compact = tagSeq.getCompact
    val end = compact.size
    @tailrec
    def makeShingles(start: Int, acc: Array[(Int, Int)]): Array[(Int, Int)] = {
      def findEnd(curRoot: TreeNode): Int = {
//        (start + 1 until end) find {
//          compact(_).depth <= curRoot.depth
//        } match {
//          case Some(x) => x
//          case _ => end
//        }
        var prev = compact(start).depth
        for (i <- start + 1 until end) {
          if (compact(i).depth <= prev)
            return i
          prev = compact(i).depth
        }
        end
      }
      if (start < end) {
        val curRoot = compact(start)
        val curRange = (start, findEnd(curRoot))         
        makeShingles(curRange._2, acc :+ curRange)
      } else {
      	acc
      }
    }
    val makeNodeArray = {
      val makeRange = Function.tupled((x: Int, y: Int) => Range(x, y))
      ((range: (Int, Int)) => makeRange(range).map(compact(_)))
    }
    val ranges = makeShingles(0, Array())
    val lastShingle = new Shingle(makeNodeArray(ranges.last), None)
    var nextShingle = Some(lastShingle)
    (for (i <- (0 until ranges.length - 1).reverse) yield {
      val shingle = new Shingle(makeNodeArray(ranges(i)), nextShingle)
      nextShingle = Some(shingle)
      shingle
    }) :+ lastShingle
  }
  override def equals(other: Any) = {
    other match {
      case that: TagSegment => 
        (that canEqual this) && this.hashCode == that.hashCode
      case _ => false
    }
  }
  def canEqual(that: Any) = that.isInstanceOf[TagSegment]
  override val hashCode = 
    41 * tagSeq.getCompact.foldLeft(1)((acc, x) => 41 * acc + x.hashCode) +
    (41 * beginIndex.hashCode + endIndex.hashCode)
  override def toString() = {
    tagSeq.getCompact.mkString(" ")
  }
}