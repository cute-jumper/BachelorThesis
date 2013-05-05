package thu.ailab.preprocess

import scala.collection.mutable.{HashMap => MHashMap, ArrayBuffer}
import scala.annotation.tailrec

class SuffixTree(val inputStr: String) {
  abstract class Show
  class Edge(val parentNode: Node, val startIndex: Int) extends Show {
    def this(_parentNode: Node, _startIndex: Int, _endNode: BaseNode, _endIndex: Int) = {
      this(_parentNode, _startIndex)
      this.endNode = _endNode
      this.endIndex = _endIndex
    }
    def getEndIndex() = if (endNode.isLeaf) endPtr else endIndex
    private var endIndex: Int = _
    var endNode: BaseNode = Leaf()
    def getCharAt(idx: Int) = inputStr(startIndex + idx)
    def closeEdge(edgeLen: Int, _startIndex: Int) = {
      endIndex = startIndex + edgeLen - 1
      val iNode = Node()
      iNode.addEdge(_startIndex)
      iNode.addEdge(inputStr(endIndex + 1), sameExceptStart(iNode, endIndex + 1))
      endNode = iNode
    }
    def sameExceptStart(_parentNode: Node, _startIndex: Int) = {
      new Edge(_parentNode, _startIndex, this.endNode, this.endIndex)
    }
    def length = getEndIndex - startIndex + 1
    
    def toDot() = {
      "\t" + parentNode.name + "->" 
    }
    def accept(visitor: SuffixTreeVisitor) = {
      visitor.visit(this)
    }
  }
  abstract class BaseNode(baseName: String, number: Int) extends Show {
    val name = baseName + number
    val isLeaf: Boolean
  }
  def InstanceBuilder[T](stm: (Int) => T) = {
    var number = -1
    () => {
      number += 1
      stm(number)
    }
  }
  object Node {
    val getInstance = InstanceBuilder(new Node(_))
    def apply() = getInstance()
  }
  object Leaf {
    val getInstance = InstanceBuilder(new Leaf(_))
    def apply() = getInstance()
  }
  class Node private(number: Int) extends BaseNode("node", number) {
    val edges = new MHashMap[Char, Edge]
    var suffixLink: Option[Node] = None 
    def hasEdge(c: Char) = edges.contains(c)
    def addEdge(idx: Int) = {
      edges(inputStr(idx)) = new Edge(this, idx)
    }
    def addEdge(c: Char, edge: Edge) = {
      edges(c) = edge
    }
    def getEdge(c: Char) = edges.get(c)
    
    val isLeaf = false
    def toDot() = {
      "\t" + name + "[label=\"\"];"
    }
    def accept(visitor: SuffixTreeVisitor) = {
      visitor.visit(this)
    }
  }
  class Leaf private(number: Int) extends BaseNode("leaf", number) {
    val isLeaf = true
  }
  /**
   * Tree variable
   */
  var endPtr: Int = -1
  val root: Node = Node()
  val activePoint = new {
    var activeNode: Node = root
    var activeEdge: Char = '\0'
    var activeLength: Int = 0
    def updateValue(c: Char) = {
      if (activeEdge == '\0' && activeNode.hasEdge(c)) {
        activeEdge = c
        activeLength += 1
        true
      } else {
        if (activeNode.hasEdge(activeEdge) && 
            activeNode.getEdge(activeEdge).get.getCharAt(activeLength) == c) {
          activeLength += 1
          true
        } else false        
      }
    }
    def insertEdge(endIndex: Int, preInsertNode: Option[Node]) = {
      if (activeEdge == '\0' || activeLength == 0) {
        activeNode.addEdge(endIndex)
        activeNode.getEdge(inputStr(endIndex)).get
      } else {
        val edge = activeNode.getEdge(activeEdge).get
        edge.closeEdge(activeLength, endIndex)
        if (preInsertNode.isDefined)
          edge.endNode.asInstanceOf[Node].suffixLink = preInsertNode
        edge
      }
    }
    def moveActivePoint() = {
      if (activeNode == root) {
        activeEdge = inputStr(endPtr - remainder + 1)
        activeLength -= 1
      } else {
        activeNode = activeNode.suffixLink.getOrElse(root)
      }
    }
    def normalize(oldEdge: Edge) = {
      @tailrec
      def doNormalize(startIndex: Int): Unit = {
        val newEdge = activeNode.getEdge(activeEdge).get
        if (newEdge.length <= activeLength) {
          assert(!newEdge.endNode.isLeaf)
          activeNode = newEdge.endNode.asInstanceOf[Node]
          activeEdge = oldEdge.getCharAt(startIndex + newEdge.length)
          activeLength -= newEdge.length
          doNormalize(startIndex + newEdge.length)
        }
      }
      doNormalize(0)
    }
  }
  var remainder: Int = 1
  // Tree variable ends
  @tailrec
  private def insertSuffix(times: Int, preInsertNode: Option[Node]): Unit = {
    def doInsert = {
      val c = inputStr(endPtr)
      if (activePoint.updateValue(c)) {
        remainder += 1
        None
      } else {
        val edge = activePoint.insertEdge(endPtr, preInsertNode)
        remainder -= 1
        activePoint.moveActivePoint
        Some(edge.endNode)
      }
    }
    if (times > 0) {
      val iNode = doInsert
      if (iNode.isDefined) insertSuffix(times - 1, iNode.foreach{_.asInstanceOf[Node]})
    }
  }
  def build {
    if (endPtr <= inputStr.length()) {
      insertSuffix(remainder, None)
      endPtr += 1
      build
    }
  }
  build
  class SuffixTreeVisitor(pw: java.io.PrintWriter) {
    def visit(show: Show) = {
      show match {
        case node: Node =>
          pw.println(node.toDot)
        case edge: Edge =>
          pw.println(edge.toDot)
      }
    }
  }

  def traverse(visitor: SuffixTreeVisitor) = {
    def doTraverse(node: Node) {
      node.accept(visitor)
    }
  }
  def translateToDot(filename: String) = {
    import thu.ailab.utils.Tools.withPrintWriter
    withPrintWriter(filename) { pw =>
      pw.print("digraph G {\n\trankdir = LR;\n\tedge[arrowsize=0.4, fontsize=10];\n\t" +
      		"node[shape=point];\n\t")
      
      pw.println("}")
    }
  }
}


object TestSuffixTree extends App {
  new SuffixTree("abc")
}