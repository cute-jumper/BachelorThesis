package thu.ailab.template

import scala.collection.mutable.LinkedList
import scala.collection.mutable.{HashMap => MHashMap}
import scala.annotation.tailrec

import thu.ailab.global.MyConfigFactory
import thu.ailab.document.{TagSequence, TagSeqFactory}
import thu.ailab.tree.TreeNode
import thu.ailab.distance.LCSWithPath

class CenterMethod(centerId: Int, fileIds: Seq[Int]) {
  val id2filename = scala.io.Source.fromFile(
      MyConfigFactory.getValue[String]("output.id2filename")).getLines.toArray
  val tagSeqFactory = new TagSeqFactory(id2filename)
  val centerTagSeq = tagSeqFactory.getInstance(centerId)
  val tagSegMap = new MHashMap[(Int, Int), Array[TagSegment]]
  for (id <- fileIds if id != centerId) {
    val otherTagSeq = tagSeqFactory.getInstance(id)
    val lcs = new LCSWithPath(centerTagSeq, otherTagSeq)
    val commonIndices = lcs.getCommonIndices
    var prevIndex = (-1, -1)  
    for (curIndex <- commonIndices) {
      if (curIndex._2 - prevIndex._2 != 1) {
        val tagSeg = new TagSegment(id, prevIndex, curIndex, otherTagSeq)
        val segment = (prevIndex._1, curIndex._1)
        tagSegMap(segment) = tagSegMap.getOrElse(segment, Array()) :+ tagSeg 
      }
      prevIndex = curIndex
    }
    if (otherTagSeq.separateLength - 1 > prevIndex._2) {
      val tagSeg = new TagSegment(id, prevIndex, (centerTagSeq.separateLength, 
          otherTagSeq.separateLength), otherTagSeq)
      val segment = (prevIndex._1, centerTagSeq.separateLength)
      tagSegMap(segment) = tagSegMap.getOrElse(segment, Array()) :+ tagSeg
    }
  }
  for ((segment, tss) <- tagSegMap) {
    val tssCount = new MHashMap[TagSegment, Int]
    for (ts <- tss) {
      tssCount(ts) = tssCount.getOrElse(ts, 0) + 1
    }
  }
  def findLCSInAll(initTs: TagSequence, fileIdIterator: Iterator[Int]) = {
    @tailrec
    def helper(lcs: TagSequence, it: Iterator[Int]): TagSequence = {
      if (it.hasNext) {
        val id = it.next
        val otherTagSeq = tagSeqFactory.getInstance(id)
        val indices = new LCSWithPath(lcs, otherTagSeq).getCommonIndices
        helper(new TagSequence(lcs.getNormalizeLCS(indices.unzip._1), true), it)
      } else {
        lcs
      }
    }
    helper(initTs, fileIdIterator)
  }
}

object TestCenterMethod extends App {
  
}