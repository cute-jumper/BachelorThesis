package thu.ailab.template

import scala.annotation.tailrec

import thu.ailab.global.MyConfigFactory
import thu.ailab.document.TagSequence
import thu.ailab.tree.TreeNode

class TagSegment(val fileId: Int, prevIndices:(Int, Int), nextIndices: (Int, Int),
    tagSeq: TagSequence) {
  val prevCenterIndex = prevIndices._1
  val nextCenterIndex = nextIndices._1
  val beginIndex = prevIndices._2 + 1
  val endIndex = nextIndices._2
  val tagSeqStored = tagSeq.getNormalizeLCS((beginIndex until endIndex).toSeq)
  lazy val shingles = {
    val sep = tagSeqStored.getSeparate
    val end = sep.size
    @tailrec
    def makeShingles(start: Int, before: Option[TreeNode], acc: Array[Shingle]): Array[Shingle] = {
      if (start < end) {
        val curRoot = sep(start)
        val curRange = Array(start) ++ ((start + 1 until end) takeWhile { x =>
          sep(x).depth > curRoot.depth
        })
        val after = if (curRange.last == end - 1) None else Some(sep(curRange.last + 1))
        val shingle = new Shingle(curRange.map(sep(_)), before, after)
        makeShingles(curRange.last + 1, Some(curRoot), acc :+ shingle)
      } else {
        acc
      }
    }
    makeShingles(0, None, Array())
  }
  override def equals(other: Any) = {
    other match {
      case that: TagSegment => 
        (that canEqual this) && this.hashCode == that.hashCode
      case _ => false
    }
  }
  def canEqual(that: Any) = that.isInstanceOf[TagSegment]
  override val hashCode = 41 * tagSeqStored.getCompact.foldLeft(1)((acc, x) =>
    41 * acc + x.hashCode) + (41 * prevCenterIndex + nextCenterIndex)
  override def toString() = {
    tagSeqStored.getCompact.mkString(" ")
  }
}