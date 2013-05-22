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
    type TreeNodeOp = Value
    val RESERVE, REMOVE, MERGE = Value
  }
  import ArrayElementOp._
  def stripDuplicates(elementSeq: Array[TreeNode]) = {
    val suffixTree = new HTMLSuffixTree(elementSeq.map(_.toString), 
        verbose = false)
    type InternalNode = suffixTree.InternalNode
    val rangeMap = suffixTree.findAllRepetitions
    val (singleRanges, longRanges) = rangeMap.partition{ x =>
      (x._1._2 - x._1._1) == 1
    }
    val flagSeq = Array.fill(elementSeq.length)(RESERVE)
    val singleRangeKeys = singleRanges.keys.toList.sortBy(_._1)
    var headRangeOption: Option[(Int, Int)] = None 
    var headNodeOption: Option[InternalNode] = None
    for (curRange <- singleRangeKeys) {
      if (headRangeOption.isEmpty) {
        headRangeOption = Some(curRange)
        headNodeOption = Some(singleRanges(curRange))
      } else {
        val curNode = singleRanges(curRange)
        if (curNode == headNodeOption.get &&
            curRange._1 - headRangeOption.get._1 == 1) {
          elementSeq(curRange._1 - 1) = new TreeNode(
              elementSeq(curRange._1 - 1),
              true)
          flagSeq(curRange._1) = REMOVE
        } else {
          headRangeOption = Some(curRange)
          headNodeOption = Some(curNode)
        }
      }
    }
    val longRangeKeys = longRanges.keys.toList.sortBy(_._1)
    val nodeAppearedBefore = new MHashSet[InternalNode]
    for (index <- 0 until longRangeKeys.length) {
      val (start, end) = longRangeKeys(index)
      val curNode = longRanges((start, end))      
      if (nodeAppearedBefore.contains(curNode)) {
        for (i <- start until end)
          flagSeq(i) = REMOVE
      } else {
        nodeAppearedBefore += curNode
        if (flagSeq(start) == REMOVE && index > 0) {
          val (preStart, preEnd) = longRangeKeys(index - 1)
          if (preEnd > start) {
            for (i <- preStart until start)
              flagSeq(i) = RESERVE
          }
        }
        flagSeq(start) = MERGE
        elementSeq(start) = TreeNode.merge(elementSeq.slice(start, end))
        for (i <- start + 1 until end)
          flagSeq(i) = REMOVE
      }
    }
    for ((node, flag) <- elementSeq zip flagSeq if flag != REMOVE) yield node
  }
}

object TestHTMLSuffixTree extends App {
  import thu.ailab.utils.Tools.timeIt
  //val s = "dedododeeodo"
  //val t1 = new SuffixTree[Char](s, '\0', '$')
  val fn = System.getProperty("user.home") + "/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_000173770100g2g7.html"
//  val tagSeq = new TreeBuilder(fn).getTagSequence.toArray
  //tagSeq.foreach(print)
  //val tagSeq = Array("html1", "body2", "a3", "body2", "a3")
  val tagSeq = Array("html1", "head2", "meta3", "meta3", "body2", "div3", 
      "div3", "div3", "div3", "div2", "div3", "a4", "div4", "img5", "div3", 
      "a4", "div4", "img5", "div3", "a4", "div4", "img5").map { x =>
    val splits = x.split(".")
    new TreeNode(splits(0), splits(1).toInt)
  }
  //val html = Array("body2", "div3", "a4", "div4", "img5", "div3", "a4", "div4", "img5", "div3")
  //val html = Array("body1", "head2", "div3", "a4", "p4", "a4", "p4", "div3", "ul4", "li5", "li5", "li5")
  val newTagSeq = HTMLSuffixTree.stripDuplicates(tagSeq)
  println(newTagSeq mkString " ")
}
