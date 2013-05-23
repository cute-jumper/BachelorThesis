package thu.ailab.tree

import scala.collection.mutable.{HashMap => MHashMap, HashSet => MHashSet}
import scala.collection.mutable.ArrayBuffer

/**
 * This class must be used when the TreeNode instance has only one tag and call
 * toString to each TreeNode
 */
private class HTMLSuffixTree(html: Array[String], verbose: Boolean = true) extends 
SuffixTree[String](html, "\0", "$.-1", verbose) {
  def parseDepth(s: String) = {
    val splits = s.split("\\.")
    splits(splits.length - 1).toInt
  }
  private def findAllRepetitions() = {
    val rangeMap = new MHashMap[Int, (Int, InternalNode)]
    def findAllRepetitionsImpl(curNode: InternalNode, 
        prefixSeq: IndexedSeq[String],
        preMinDepth: Int): Boolean = {
      val (usefulEdges, uselessEdges) = curNode.edges.partition(x => 
        parseDepth(x._1) >= preMinDepth &&
          x._2.isClosed)
      for {
        (key, curEdge) <- usefulEdges
        curMinDepth = parseDepth(key)
      } {
        val edgeSeq = curEdge.getEdgeSeq
        val nextNode = curEdge.endNode.asInstanceOf[InternalNode]
        var rep = edgeSeq.takeWhile(parseDepth(_) >= curMinDepth)
        if (rep.length < edgeSeq.length || 
            !findAllRepetitionsImpl(nextNode, prefixSeq ++ edgeSeq,
            parseDepth(edgeSeq(edgeSeq.length - 1)))) {
          for (beginIndex <- curEdge.ranges) {
            val endIndex = beginIndex + rep.length
            val newLength = prefixSeq.length + rep.length
            if (rangeMap.contains(endIndex)) {
              val origLength = rangeMap.get(endIndex).get._1
              if (origLength < newLength)
                rangeMap(endIndex) = (newLength, nextNode) 
            } else {
              rangeMap(endIndex) = (newLength, nextNode)
            }
          }
        }
      }
      if (curNode != root) {
        for {
          (key, curEdge) <- uselessEdges
          newLength = prefixSeq.length
          beginIndex <- curEdge.ranges
        } {
          if (rangeMap.contains(beginIndex)) {
            val origLength = rangeMap.get(beginIndex).get._1
            if (origLength < newLength)
              rangeMap(beginIndex) = (newLength, curNode)
          } else {
            rangeMap(beginIndex) = (newLength, curNode)
          }
        }
      }
      usefulEdges.size != 0
    }
    findAllRepetitionsImpl(root, IndexedSeq[String](), 0)
    rangeMap.map { x =>
      (x._1 - x._2._1, x._1) -> x._2._2
    }
  }
}

object HTMLSuffixTree {
  object ArrayElementOp extends Enumeration {
    type ArrayElementOp = Value
    val RESERVE, REMOVE, MERGE = Value
  }
  import ArrayElementOp._
  def stripDuplicates(elementSeq: Array[TreeNode]) = {
    val suffixTree = new HTMLSuffixTree(elementSeq.map(_.toString), 
        verbose = false)
    type InternalNode = suffixTree.InternalNode
    val rangeMap = suffixTree.findAllRepetitions
    val sortedRanges = rangeMap.keys.toSeq.sortBy(-_._1)
    val fatherMap = new MHashMap[TreeNode, List[(Int, Int)]]
    val flagArray = Array.fill(elementSeq.length)(RESERVE)
    for (range <- sortedRanges) {
      val curDepth = elementSeq(range._1).depth
      val father = elementSeq((range._1 to 0 by -1).iterator.dropWhile{ x =>
        elementSeq(x).depth >= curDepth
      }.next)
      fatherMap(father) = range :: fatherMap.getOrElse(father, Nil)
    }
    for (rangeList <- fatherMap.values if rangeList.length > 1) {
      val rangeCount = new MHashMap[InternalNode, Int]
      for (range <- rangeList; node = rangeMap(range))
        rangeCount(node) += rangeCount.getOrElse(node, 0) + 1
      var preRange = (0, 0)
      for (range <- rangeList; node = rangeMap(range) if rangeCount(node) > 1;
      (start, end) = range) {
        val nodeOccursBefore = new MHashSet[InternalNode]
        if (nodeOccursBefore.contains(node)) {
          for (i <- start until end)
            flagArray(i) = REMOVE
        } else {
          nodeOccursBefore += node
          if (flagArray(start) == REMOVE && preRange._2 > start)
            for (i <- preRange._1 until start)
              flagArray(i) = RESERVE
        }
        preRange = range
        flagArray(start) = MERGE
        elementSeq(start) = TreeNode.merge(elementSeq.slice(start, end))
        for (i <- start + 1 until end)
          flagArray(i) = REMOVE
        println(range)
      }
      println("============")
    }
    for ((node, flag) <- elementSeq zip flagArray if flag != REMOVE) yield node
  }
}

object TestHTMLSuffixTree extends App {
  import thu.ailab.utils.Tools.timeIt
  //val s = "dedododeeodo"
  //val t1 = new SuffixTree[Char](s, '\0', '$')
  val fn = System.getProperty("user.home") + "/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_002b5d980100szxu.html"
  val tagSeq = new TreeBuilder(fn).getTagSequence.toArray
  println(tagSeq mkString " ")
  //tagSeq.foreach(print)
  //val tagSeq = Array("html1", "body2", "a3", "body2", "a3")
//  val tagSeq = Array("html1", "head2", "meta3", "meta3", "body2", "div3", 
//      "div3", "div3", "div3", "div2", "div3", "a4", "div4", "img5", "div3", 
//      "a4", "div4", "img5", "div3", "a4", "div4", "img5").map { x =>
//    val splits = x.split(".")
//    new TreeNode(splits(0), splits(1).toInt)
//  }
  //val html = Array("body2", "div3", "a4", "div4", "img5", "div3", "a4", "div4", "img5", "div3")
  //val html = Array("body1", "head2", "div3", "a4", "p4", "a4", "p4", "div3", "ul4", "li5", "li5", "li5")
  val newTagSeq = HTMLSuffixTree.stripDuplicates(tagSeq)
  println(newTagSeq mkString " ")
}
