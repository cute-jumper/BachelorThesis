package thu.ailab.preprocess

import scala.collection.mutable.{HashMap => MHashMap}

class SuffixTree(val inputStr: String) {
  class Edge(val startIndex: Int) {
    def endIndex: Int = endPtr
    val endNode: Option[Node] = None
    def getChar(idx: Int) = inputStr(startIndex + idx)
  }
  class Node {
    val edges = new MHashMap[Char, Edge]
    def hasEdge(c: Char) = edges.contains(c)
    def addEdge(endPtr: Int) = {
      edges(inputStr(endPtr)) = new Edge(endPtr)
    }
    def getEdge(c: Char) = edges.get(c)
  }
  /**
   * Tree variable
   */
  var endPtr: Int = -1
  val root: Node = new Node
  val activePoint = new {
    var activeNode: Node = _
    var activeEdge: Char = '\0'
    var activeLength: Int = 0
    def hasChar(c: Char) = {
      if (!activeNode.hasEdge(c)) false
      else { 
        val edge = activeNode.getEdge(c).get
        edge.getChar(activeLength) == c
      }
    }
  }
  var remainder: Int = 1
  // Tree variable ends
  def insertSuffix = {
    val activeNode = activePoint.activeNode
    val c = inputStr(endPtr)
    if (activeNode.hasEdge(c)) {
      activePoint.activeEdge = c
      activePoint.activeLength += 1
      remainder += 1
    } else {
      activeNode.addEdge(endPtr)
    }
  }
  
}

object TestSuffixTree extends App {
  new SuffixTree("abc")
}