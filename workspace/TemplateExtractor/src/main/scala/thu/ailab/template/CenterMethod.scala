package thu.ailab.template

import scala.collection.mutable.LinkedList
import scala.collection.mutable.{HashMap => MHashMap}
import scala.annotation.tailrec
import thu.ailab.global.MyConfigFactory
import thu.ailab.sequence._
import thu.ailab.tree.TreeNode
import thu.ailab.distance.LCSWithPath
import thu.ailab.cluster.NaiveAggloCluster

class CenterMethod(centerId: Int, fileIds: Seq[Int]) {
  val id2filename = scala.io.Source.fromFile(
      MyConfigFactory.getValue[String]("output.id2filename")).getLines.toArray
  val tagSeqFactory = new TagSeqFactory(id2filename)
  val centerTagSeq = tagSeqFactory.getInstance(centerId)
  val clcs = findLCSInAll(centerTagSeq, fileIds.filter(_ != centerId).iterator)
  println(clcs.getCompact.mkString(" "))
  val clcsLen = clcs.getCompact.length
  val tagSegMap = new MHashMap[(Int, Int), Array[TagSegment]]
  for (id <- fileIds) {
    val otherTagSeq = tagSeqFactory.getInstance(id)
    val lcs = new LCSWithPath(clcs, otherTagSeq)
    val commonIndices = lcs.getCommonIndices
    assert(clcsLen == commonIndices.length)
    var prevIndex = (-1, -1)  
    for (curIndex <- commonIndices) {
      if (curIndex._2 - prevIndex._2 != 1) {
        val tagSeg = new TagSegment(otherTagSeq, prevIndex._2, curIndex._2, id)
        val range = (prevIndex._1, curIndex._1)
        tagSegMap(range) = tagSegMap.getOrElse(range, Array()) :+ tagSeg 
      }
      prevIndex = curIndex
    }
    if (otherTagSeq.separateLength - 1 > prevIndex._2) {
      val tagSeg = new TagSegment(otherTagSeq, prevIndex._2,
          otherTagSeq.separateLength, id)
      val range = (prevIndex._1, clcs.separateLength)
      tagSegMap(range) = tagSegMap.getOrElse(range, Array()) :+ tagSeg
    }
  }
  for ((range, tss) <- tagSegMap) {
//    val shingleCount = new MHashMap[Shingle, Int]
//    for (ts <- tss; shingle <- ts.shingles) {
//      shingleCount(shingle) = shingleCount.getOrElse(shingle, 0) + 1
//    }
//    val minShingleCount = 500
//    val ks = shingleCount.filter(_._2 > minShingleCount).keySet
//    if (ks.exists(_.before == None) && ks.exists(_.after == None)) {
//      val pf = new PathFinder(ks.toSet)
//    }
//    println("=" * 80)
//    println(range)
//    for ((ts, idx) <- tss.zipWithIndex) {
//      println(
//          ts.shingles.map { s => 
//            "%4d:%s".format(shingleCount(s), s.toString)
//          } mkString " ")
//    }
//    println(shingleCount.keys.toSeq.sortBy(shingleCount(_)).map(
//        x => "%4d:%s".format(shingleCount(x), x)).mkString("\n"))    
//    println(shingleCount.groupBy(x => x._1.getNext).map(
//        m => m._2.keys.toSeq.sortBy(x => -shingleCount(x))
//        .map(k => "%4d:".format(shingleCount(k)) + k.getNext + " | " + k.getMain.mkString(" "))
//        .mkString("\n")).mkString("\n++++++\n"))
  }
  def findLCSInAll(initTs: TagSequence, fileIdIterator: Iterator[Int]) = {
    @tailrec
    def helper(lcs: TagSequence, it: Iterator[Int]): TagSequence = {
      if (it.hasNext) {
        val id = it.next
        val otherTagSeq = tagSeqFactory.getInstance(id)
        val indices = new LCSWithPath(lcs, otherTagSeq).getCommonIndices
        helper(lcs.getNormalizeLCS(indices.unzip._1), it)
      } else {
        lcs
      }
    }
    helper(initTs, fileIdIterator)
  }
}

object TestCenterMethod extends App {
  
}