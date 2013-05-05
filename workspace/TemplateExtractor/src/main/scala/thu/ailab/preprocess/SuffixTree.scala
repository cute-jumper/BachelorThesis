package thu.ailab.preprocess

import scala.collection.mutable.{HashMap => MHashMap, ArrayBuffer}
import scala.annotation.tailrec

object RenderChars {
  val TJunctionDown  = '┬';
  val HorizontalLine = '─';
  val VerticalLine   = '│';
  val TJunctionRight = '├';
  val CornerRight    = '└';
}

class SuffixTree(val inputStr: String) {
  abstract class Show
  class Edge(val parentNode: InternalNode, val beginIndex: Int) extends Show {
    def this(_parentNode: InternalNode, _beginIndex: Int, _endNode: BaseNode, 
        _endIndex: Int) = {
      this(_parentNode, _beginIndex)
      this.endNode = _endNode
      this.endIndex = _endIndex
    }
    def getEndIndex() = if (endNode.isLeaf) endPtr else endIndex
    private var endIndex: Int = _
    var endNode: BaseNode = Leaf()
    def getCharAt(idx: Int) = inputStr(beginIndex + idx)
    def closeEdge(edgeLen: Int, _beginIndex: Int) = {
      endIndex = beginIndex + edgeLen - 1
      val iNode = InternalNode()
      iNode.addEdge(_beginIndex)
      iNode.addEdge(inputStr(endIndex + 1), 
          sameExceptStart(iNode, endIndex + 1))
      endNode = iNode
    }
    def sameExceptStart(_parentNode: InternalNode, _beginIndex: Int) = {
      new Edge(_parentNode, _beginIndex, this.endNode, this.endIndex)
    }
    def length = getEndIndex - beginIndex + 1
    
    def toAscii() = {
      
    }
    def toDot() = {
      "\t" + parentNode.name + "->" + endNode.name + 
      "[label=\"%s\"];".format(inputStr.substring(beginIndex, endIndex))
    }
    def accept(visitor: SuffixTreeVisitor) = {
      visitor.visit(this)
    }
  }
  abstract class BaseNode(baseName: String, number: Int) extends Show {
    val name = baseName + number
    val isLeaf: Boolean
  }
  private def InstanceBuilder[T](stm: (Int) => T) = {
    var number = -1
    () => {
      number += 1
      stm(number)
    }
  }
  object InternalNode {
    val getInstance = InstanceBuilder(new InternalNode(_))
    def apply() = getInstance()
  }
  object Leaf {
    val getInstance = InstanceBuilder(new Leaf(_))
    def apply() = getInstance()
  }
  class InternalNode private(number: Int) extends BaseNode("node", number) {
    val edges = new MHashMap[Char, Edge]
    var suffixLink: Option[InternalNode] = None 
    def hasEdge(c: Char) = edges.contains(c)
    def addEdge(idx: Int) = {
      edges(inputStr(idx)) = new Edge(InternalNode.this, idx)
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
      visitor.visit(InternalNode.this)
    }
  }
  class Leaf private(number: Int) extends BaseNode("leaf", number) {
    val isLeaf = true
  }
  /**
   * Tree variable
   */
  var endPtr: Int = -1
  val root: InternalNode = InternalNode()
  val activePoint = new {
    var activeNode: InternalNode = root
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
    def insertEdge(endIndex: Int, preInsertNode: Option[InternalNode]) = {
      if (activeEdge == '\0' || activeLength == 0) {
        activeNode.addEdge(endIndex)
        activeNode.getEdge(inputStr(endIndex)).get
      } else {
        val edge = activeNode.getEdge(activeEdge).get
        edge.closeEdge(activeLength, endIndex)
        if (preInsertNode.isDefined)
          edge.endNode.asInstanceOf[InternalNode].suffixLink = preInsertNode
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
      def doNormalize(beginIndex: Int): Unit = {
        val newEdge = activeNode.getEdge(activeEdge).get
        if (newEdge.length <= activeLength) {
          assert(!newEdge.endNode.isLeaf)
          activeNode = newEdge.endNode.asInstanceOf[InternalNode]
          activeEdge = oldEdge.getCharAt(beginIndex + newEdge.length)
          activeLength -= newEdge.length
          doNormalize(beginIndex + newEdge.length)
        }
      }
      doNormalize(0)
    }
  }
  var remainder: Int = 1
  // Tree variable ends
  @tailrec
  private def insertSuffix(times: Int, 
      preInsertNode: Option[InternalNode]): Unit = {
    def doInsert = {
      val c = inputStr(endPtr)
      if (activePoint.updateValue(c)) {
        remainder += 1
        None
      } else {
        val edge = activePoint.insertEdge(endPtr, preInsertNode)
        remainder -= 1
        activePoint.moveActivePoint
        Some(edge.endNode.asInstanceOf[InternalNode])
      }
    }
    if (times > 0) {
      val iNode = doInsert
      if (iNode.isDefined) insertSuffix(times - 1, iNode)
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
        case node: InternalNode =>
          pw.println(node.toDot)
        case edge: Edge =>
          pw.println(edge.toDot)
      }
    }
  }

  def traverse(visitor: SuffixTreeVisitor) = {
    def doTraverse(node: InternalNode) {
      node.accept(visitor)
    }
  }
  def translateToDot(filename: String) = {
    import thu.ailab.utils.Tools.withPrintWriter
    withPrintWriter(filename) { pw =>
      pw.println("digraph G {\n\trankdir = LR;\n" +
      		"\tedge[arrowsize=0.4, fontsize=10];\n\t" +
      		"node[shape=point];")
      traverse(new SuffixTreeVisitor(pw))
      pw.println("}")
    }
  }
}

object TestSuffixTree extends App {
  new SuffixTree("abc")
}