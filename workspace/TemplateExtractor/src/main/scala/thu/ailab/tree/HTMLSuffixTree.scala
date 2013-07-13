package thu.ailab.tree

import scala.collection.mutable.{HashMap => MHashMap, HashSet => MHashSet}
import scala.collection.mutable.ArrayBuffer
import thu.ailab.global.LoggerTrait

/**
 * This class must be used when the TreeNode instance has only one tag and call
 * toString to each TreeNode
 */
private class HTMLSuffixTree(html: Array[String], verbose: Boolean = true) extends 
SuffixTree[String](html, "\0", "$.-1", verbose) {
  def parseDepth(s: String) = s.split("\\.").last.toInt
  private def findAllRepetitions() = {
    val rangeMap = new MHashMap[Int, (Int, InternalNode)]
    /**
     * A common pattern to update a Map
     */
    def updateRangeMap(endIndex: Int, newLength: Int, node: InternalNode) = {
      if (rangeMap.contains(endIndex)) {
        val origLength = rangeMap.get(endIndex).get._1
        if (origLength < newLength)
          rangeMap(endIndex) = (newLength, node) 
      } else {
        rangeMap(endIndex) = (newLength, node)
      }
    }
    /**
     * Recursive implementation
     */
    def findAllRepetitionsImpl(curNode: InternalNode, 
        prefixSeq: IndexedSeq[String],
        preMinDepth: Int): Unit = {
      /**
       * Partition edges by the depth
       */
      val (contEdges, stopEdges) = curNode.edges.partition(x => 
        parseDepth(x._1) > preMinDepth)
      /**
       * Iterate each "contEdges" which has a closed node
       */
      for {
        (key, curEdge) <- contEdges if curEdge.isClosed
        nextNode = curEdge.endNode.asInstanceOf[InternalNode]
        curMinDepth = parseDepth(key)
      } {
        val edgeSeq = curEdge.getEdgeSeq
        var rep = edgeSeq.takeWhile(parseDepth(_) >= curMinDepth)
        /**
         * Not reach the end --> stop and update the Map
         * Reach the end --> continue by recursively calling the function.
         */
        if (rep.length < edgeSeq.length) {
          for (beginIndex <- curEdge.ranges) {
            updateRangeMap(beginIndex + rep.length, 
                prefixSeq.length + rep.length,
                nextNode)
          }
        } else {
          findAllRepetitionsImpl(nextNode, prefixSeq ++ edgeSeq,
              /* Here contains "Bounds Checking" */
              if (preMinDepth == -1) parseDepth(edgeSeq.head) else preMinDepth)
        }
      }
      /**
       * Iterate each "stopEdges" and update the Map
       */
      for ((key, curEdge) <- stopEdges; endIndex <- curEdge.ranges)
        updateRangeMap(endIndex, prefixSeq.length, curNode)
    }
    findAllRepetitionsImpl(root, IndexedSeq[String](), -1)
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
  
  /**
   * Statistics
   */
  val statMap = new MHashMap[Int, Int]  
  def clearStat() = statMap.clear()
  def getStat() = statMap.toMap
  
  /**
   * Group nodes by their fathers
   */
  private def getFatherMap[T <: TreeNode](ranges: Iterable[(Int, Int)], 
      tnArray: Array[T]) = {    
    val sortedRanges = ranges.toSeq.sortBy(-_._1)
    val fatherMap = new MHashMap[(T, Int), List[(Int, Int)]]
    for (range <- sortedRanges) {
      val curDepth = tnArray(range._1).depth
      /**
       * Iterate to find the index of the father
       */
      val fatherIndex = (range._1 to 0 by -1).iterator.dropWhile{
        tnArray(_).depth >= curDepth
      }.next
      val fatherNode = tnArray(fatherIndex)
      fatherMap((fatherNode, fatherIndex)) = range :: 
      fatherMap.getOrElse((fatherNode, fatherIndex), Nil)
    }
    fatherMap.toMap
  }
  /**
   * Two functions below are quite similar. The first one should be
   * used with "Array of TreeNode", while the second one should be
   * used with "Array of VerboseTreeNode". The main difference is how
   * to update state variables when iterating each range.
   * 
   * The first one is mainly used for template building system, and the
   * second is used for content extracting system. 
   */
  // The first one
  def stripDuplicates(tnArray: Array[TreeNode], logStat: Boolean = false) = {
    val suffixTree = new HTMLSuffixTree(tnArray.map(_.toString), 
        verbose = false)
    val rangeMap = suffixTree.findAllRepetitions()
    val fatherMap = getFatherMap(rangeMap.keys, tnArray)
    type InternalNode = suffixTree.InternalNode
    /**
     * The boolean array indicates which nodes should be reserved
     */
    val flagArray = Array.fill(tnArray.length)(RESERVE)
    
    for (rangeList <- fatherMap.values if rangeList.length > 1) {
      val rangeCount = new MHashMap[InternalNode, Int]
      /**
       * Calculate the occurring count of each repeated sequence
       */
      for (range <- rangeList; node = rangeMap(range))
        rangeCount(node) = rangeCount.getOrElse(node, 0) + 1
      val nodeOccursBefore = new MHashSet[InternalNode]
      var preRange = (0, 0)
      /**
       * Filter out the ones whose occurring count is not more than one
       */
      for (range <- rangeList; node = rangeMap(range) if rangeCount(node) > 1;
      (start, end) = range) {
        if (nodeOccursBefore.contains(node)) {
          /**
           * if it is not the first time we see this node, mark all to be removed.
           */
          start until end foreach {flagArray(_) = REMOVE}
        } else {
          /**
           * if it is the first time we see this node, add it to the set of 
           * "already occurs"
           */
          nodeOccursBefore += node
          /**
           * if the node has already been mark as "REMOVE", this means two
           * sequences supposed to be removed overlapped. Then we should undo
           * the previous operation which mark the node as "REMOVE".
           */
          if (flagArray(start) == REMOVE && preRange._2 > start)
            preRange._1 until start foreach {flagArray(_) = RESERVE}
          /**
           * Merge all nodes to one node, place the merging node at the first
           * index and mark other nodes as "REMOVE".
           */
          flagArray(start) = MERGE
          tnArray(start) = tnArray(start).merge(tnArray.slice(start + 1, end))
          start + 1 until end foreach {flagArray(_) = REMOVE}
        }
        preRange = range
        if (logStat) {
          val key = range._2 - range._1
          statMap(key) = statMap.getOrElse(key, 0) + 1
        }
      }
    }
    for ((node, flag) <- tnArray zip flagArray if flag != REMOVE) yield node
  }
  // The second one, similar to the above one
  def stripDuplicates(vtnArray: Array[VerboseTreeNode]) = {
    val suffixTree = new HTMLSuffixTree(vtnArray.map(_.toString), 
        verbose = false)
    val rangeMap = suffixTree.findAllRepetitions()
    val fatherMap = getFatherMap(rangeMap.keys, vtnArray)
    type InternalNode = suffixTree.InternalNode
    val flagArray = Array.fill(vtnArray.length)(RESERVE)
    for (rangeList <- fatherMap.values if rangeList.length > 1) {
      val rangeCount = new MHashMap[InternalNode, Int]
      for (range <- rangeList; node = rangeMap(range))
        rangeCount(node) = rangeCount.getOrElse(node, 0) + 1
      val nodeOccursBefore = new MHashSet[InternalNode]
      var preRange = (0, 0)
      /**
       * Record where we store the merged node
       */
      val mergeTable = new MHashMap[InternalNode, Int]
      for (range <- rangeList; node = rangeMap(range) if rangeCount(node) > 1;
      (start, end) = range) {
        if (nodeOccursBefore.contains(node)) {
          start until end foreach {flagArray(_) = REMOVE}
          /**
           * Get the corresponding mergeNode and add all the Jsoup nodes to it
           */
          val mergeNode = vtnArray(mergeTable(node))
          mergeNode.addRelatedRoot(vtnArray(start))
        } else {
          nodeOccursBefore += node
          // UNDO!!
          if (flagArray(start) == REMOVE && preRange._2 > start) {
            preRange._1 until start foreach {flagArray(_) = RESERVE}
            val prevMergeNode = vtnArray(mergeTable(rangeMap(preRange)))
            prevMergeNode.removeLastRelatedRoot
          }
          flagArray(start) = MERGE
          mergeTable += node -> start
          vtnArray(start) = vtnArray(start).merge(vtnArray.slice(start + 1, end))
          start + 1 until end foreach {flagArray(_) = REMOVE}
        }
        preRange = range
      }
    }
    for ((node, flag) <- vtnArray zip flagArray if flag != REMOVE) yield node
  }
}

object TestHTMLSuffixTree extends App {
  import thu.ailab.utils.Tools.timeIt
  //val s = "BANANA"
  //val t1 = new SuffixTree[Char](s, '\0', '$')
  //t1.translateToDot(sys.props("user.home") + "/tmp/st.dot")
  //val fn = System.getProperty("user.home") + "/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_002b5d980100szxu.html"
  //val tagSeq = new TreeBuilder(fn).getTagSequence.toArray
  //println(tagSeq mkString " ")
  //tagSeq.foreach(print)
  //val tagSeq = Array("html1", "body2", "a3", "body2", "a3")
  val tagSeq = Array("html.1", "head.2", "meta.3", "meta.3", "body.2", "div.3",
      "div.3", "div.4", 
      "a.4", "div.4", "img.5", "a.4", "div.4", "img.5").map { x =>
    val splits = x.split("\\.")
    new TreeNode(splits(0), splits(1).toInt)
  }
  val t1 = new SuffixTree[String](Array("html1", "head2", "meta3", "meta3", "body2",
      "div3", "a4", "div4", "img5", "a4", "div4", "img5"), "\0", "$")
//  val tagSeq = Array("div.6", "div.7", "div.8", "span.9", "div.8", "div.9", "div.10", 
//      "div.11", "a.12", "span.13", "div.10", "span.11", "a.12", "div.8", 
//      "div.7", "div.8", "span.9", "div.8", "div.9", "div.10", "div.10", "span.11", 
//      "a.12", "div.8", "div.7", "div.8", "span.9", "span.9", "div.8", "div.9", 
//      "div.8", "div.6", "div.7", "div.8").map { x => 
//    val splits = x.split("\\.")
//    new TreeNode(splits(0), splits(1).toInt)
//  }
  //val html = Array("body2", "div3", "a4", "div4", "img5", "div3", "a4", "div4", "img5", "div3")
  //val html = Array("body1", "head2", "div3", "a4", "p4", "a4", "p4", "div3", "ul4", "li5", "li5", "li5")
  val newTagSeq = HTMLSuffixTree.stripDuplicates(tagSeq)
  println(newTagSeq mkString " ")
}
