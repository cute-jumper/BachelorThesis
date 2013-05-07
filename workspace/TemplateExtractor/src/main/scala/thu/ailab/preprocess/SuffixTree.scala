package thu.ailab.preprocess

import scala.collection.mutable.{HashMap => MHashMap, ArrayBuffer}
import scala.annotation.tailrec
import thu.ailab.utils.Tools.withPrintWriter

object RenderChars {
  val TJunctionDown  = "┬";
  val HorizontalLine = "─";
  val VerticalLine   = "│";
  val TJunctionRight = "├";
  val CornerRight    = "└";
}

class SuffixTree[T : Ordering](rawInputSeq: IndexedSeq[T], 
    val nullEdge: T, 
    val canonicalChar: T) {
  val inputSeq =
    if (rawInputSeq.indexOf(
        rawInputSeq(rawInputSeq.length - 1)) < rawInputSeq.length - 1)
      rawInputSeq :+ canonicalChar
    else rawInputSeq
  abstract class Show {
    def accept(visitor: SuffixTreeVisitor)
  }
  class Edge(val parentNode: InternalNode, val beginIndex: Int) extends Show {
    def this(_parentNode: InternalNode, _beginIndex: Int, _endNode: BaseNode, 
        _fixEndIndex: Int) = {
      this(_parentNode, _beginIndex)
      this.endNode = _endNode
      this.fixEndIndex = _fixEndIndex
    }
    def getEndIndex() = if (endNode.isLeaf) endPtr + 1 else fixEndIndex
    private var fixEndIndex: Int = 0
    var endNode: BaseNode = Leaf()
    def getCharAt(idx: Int) = inputSeq(beginIndex + idx)
    def splitEdge(edgeLen: Int, _beginIndex: Int) = {
      val iNode = InternalNode()
      //before any modification
      val save = sameExceptStart(iNode, beginIndex + edgeLen)
      fixEndIndex = beginIndex + edgeLen
      iNode.addEdge(_beginIndex)
      iNode.addEdge(inputSeq(fixEndIndex), save)
      endNode = iNode
    }
    def sameExceptStart(_parentNode: InternalNode, _beginIndex: Int) = {
      new Edge(_parentNode, _beginIndex, this.endNode, this.fixEndIndex)
    }
    def length = getEndIndex - beginIndex
    def getEdgeSeq = inputSeq.slice(beginIndex, getEndIndex)
    def getEdgeString = inputSeq.slice(beginIndex, getEndIndex).mkString(" ")
    def getEdgeLength = getEdgeString.length
    def isClosed = endNode.isInstanceOf[InternalNode]
    
    def toAscii(prefix: String, maxEdgeLength: Int) = {
      getEdgeString + (endNode match {
        case node: InternalNode =>
          RenderChars.HorizontalLine * (maxEdgeLength - getEdgeLength + 1) + 
          node.toAscii(prefix + " " * (maxEdgeLength + 1))
        case _ => ""
      })
    }
    def toDot() = {
      "\t" + parentNode.name + "->" + endNode.name + 
      "[label=\"%s\"];".format(getEdgeString)
    }
    def accept(visitor: SuffixTreeVisitor) = {
      visitor.visit(Edge.this)
    }
  }
  abstract class BaseNode(baseName: String, number: Int) extends Show {
    val name = baseName + number
    val isLeaf: Boolean
    def accept(visitor: SuffixTreeVisitor) = {
      visitor.visit(this)
    }
    def toDot() = {
      "\t" + name + "[label=\"\"];"
    }
  }
  private def InstanceBuilder[T](stm: (Int) => T) = {
    var number = -1
    (() => {
      number += 1
      stm(number)
    }, () => number)
  }
  object InternalNode {
    val (getInstance, getNumber) = InstanceBuilder(new InternalNode(_))
    def apply() = getInstance()
    def getTotalInternalNodeCount = getNumber() + 1
  }
  object Leaf {
    val (getInstance, getNumber) = InstanceBuilder(new Leaf(_))
    def apply() = getInstance()
    def getTotalLeafCount = getNumber() + 1
  }
  class InternalNode private(number: Int) extends BaseNode("node", number) {
    val edges = new MHashMap[T, Edge]
    var suffixLink: Option[InternalNode] = None 
    def hasEdge(c: T) = edges.contains(c)
    def addEdge(idx: Int) = {
      edges(inputSeq(idx)) = new Edge(InternalNode.this, idx)
    }
    def addEdge(c: T, edge: Edge) = {
      edges(c) = edge
    }
    def getEdge(c: T) = edges.get(c)
    def getNodeLabel = "(" + name.filter(_.isDigit) + ")"
    
    val isLeaf = false
    def toAscii(prefix: String): String = {
      val nodeLabel = getNodeLabel + (
          if (suffixLink.isDefined) "->" + suffixLink.get.getNodeLabel 
          else "")
      val maxEdgeLength = edges.values.maxBy(_.getEdgeLength).getEdgeLength
      val prefixPadding = prefix + " " * nodeLabel.length
      nodeLabel + (if (edges.size == 1) {
        RenderChars.HorizontalLine * 2 +
        edges.values.headOption.get.toAscii(prefixPadding + "  ", maxEdgeLength)
      }
      else {
        (for ((edge, idx) <- edges.values.toList.sortBy(
            edge => inputSeq(edge.beginIndex)).zipWithIndex) yield {
          if (idx == 0) {
            RenderChars.TJunctionDown + RenderChars.HorizontalLine + 
            edge.toAscii(prefixPadding + RenderChars.VerticalLine + " ", maxEdgeLength)
          }
          else if (idx == edges.size - 1) {
            prefixPadding + RenderChars.CornerRight + RenderChars.HorizontalLine +
            edge.toAscii(prefixPadding + "  ", maxEdgeLength)
          }
          else 
            prefixPadding + RenderChars.TJunctionRight + RenderChars.HorizontalLine +
            edge.toAscii(prefixPadding + RenderChars.VerticalLine + " ", maxEdgeLength)
        }).mkString("\n")
      })
    }
  }
  class Leaf private(number: Int) extends BaseNode("leaf", number) {
    val isLeaf = true
  }
  /**
   * Tree variable
   */
  var endPtr = 0
  val root = InternalNode()
  val activePoint = new ActivePoint
  class ActivePoint {
    var activeNode = root
    var activeEdge = nullEdge
    var activeLength = 0
    def tryInsert(c: T) = {
      var ret = false
      if (activeEdge == nullEdge && activeNode.hasEdge(c)) {
        activeEdge = c
        activeLength += 1
      } else {
        if (activeNode.hasEdge(activeEdge) &&
          activeNode.getEdge(activeEdge).get.getCharAt(activeLength) == c) {
          activeLength += 1
        } else {
          ret = true
        }
      }
      normalizeAfterUpdate
      ret
    }
    def insertEdge(curEndIndex: Int, preInsertNode: Option[InternalNode]) = {
      if (activeEdge == nullEdge || activeLength == 0) {
        activeNode.addEdge(curEndIndex)
        activeNode.getEdge(inputSeq(curEndIndex)).get.endNode
      } else {
        val edge = activeNode.getEdge(activeEdge).get
        edge.splitEdge(activeLength, curEndIndex)
        if (preInsertNode.isDefined)
          preInsertNode.get.suffixLink = Some(edge.endNode.asInstanceOf[InternalNode])
        edge.endNode
      }
    }
    def moveActivePoint(endNode: BaseNode) = {
      val oldEdgeOption = activeNode.getEdge(activeEdge)
      if (activeNode == root) {
        if (activeLength > 0) {
          activeLength -= 1
          if (activeLength == 0) activeEdge = nullEdge
          else activeEdge = inputSeq(endPtr - remainder + 1)
          if (oldEdgeOption.isDefined)
            normalizeAfterSuffix(oldEdgeOption.get, 1)
        }
      } else {
        activeNode = activeNode.suffixLink.getOrElse(root)
        if (oldEdgeOption.isDefined) normalizeAfterSuffix(oldEdgeOption.get, 0)
      }
      if (!endNode.isLeaf) 
        endNode.asInstanceOf[InternalNode].suffixLink = Some(activeNode)
    }
    def normalizeAfterUpdate() = {
      if (activeNode.hasEdge(activeEdge)) {
        val edge = activeNode.getEdge(activeEdge).get
        if (activeLength == edge.length && edge.isClosed) {
          activeNode = edge.endNode.asInstanceOf[InternalNode]
          activeEdge = nullEdge
          activeLength = 0
        }
      }
    }
    def normalizeAfterSuffix(oldEdge: Edge, initialIndex: Int) = {
      @tailrec
      def doNormalize(beginIndex: Int): Unit = {
        if (activeNode.hasEdge(activeEdge)) {
          val newEdge = activeNode.getEdge(activeEdge).get
          if (newEdge.length <= activeLength) {
            assert(!newEdge.endNode.isLeaf)
            activeNode = newEdge.endNode.asInstanceOf[InternalNode]
            activeLength -= newEdge.length
            if (activeLength == 0) activeEdge = nullEdge
            else activeEdge = oldEdge.getCharAt(beginIndex + newEdge.length)
            doNormalize(beginIndex + newEdge.length)
          }
        }
      }
      doNormalize(initialIndex)
    }
    override def toString() = {
      List("# activeNode: " + activeNode.name, 
          "# activeEdge: " + activeEdge.toString, 
          "# activeLength: " + activeLength.toString).mkString("\n") 
    }
  }
  var remainder: Int = 1
  // Tree variable ends
  @tailrec
  private def insertSuffix(times: Int, 
      preInsertNode: Option[InternalNode]): Unit = {
    def doInsert = {
      val c = inputSeq(endPtr)
      if (!activePoint.tryInsert(c)) {
        remainder += 1
        None
      } else {
        val endNode = activePoint.insertEdge(endPtr, preInsertNode)
        if (remainder > 1) remainder -= 1
        activePoint.moveActivePoint(endNode)
        Some(endNode)
      }
    }
    if (times > 0) {
      doInsert match {
        case Some(internalNode: InternalNode) => insertSuffix(times - 1, Some(internalNode))
        case Some(leaf: Leaf) => insertSuffix(times - 1, preInsertNode)
        case _ =>
      }
    }
  }
  def build {
    if (endPtr < inputSeq.length) {
      println(activePoint)
      println("# remainder: " + remainder)
      insertSuffix(remainder, None)
      println("insert %s at %d".format(inputSeq(endPtr), endPtr))
      translateToAscii
      endPtr += 1
      build
    } else {
      endPtr -= 1
    }
  }
  build
  assert(inputSeq.length == getLeafCount(root))
  def getLeafCount(node: InternalNode): Int = {
    (for (edge <- node.edges.values) yield {
      if (edge.isClosed)
        getLeafCount(edge.endNode.asInstanceOf[InternalNode])
      else
        1
    }).sum
  }
  class SuffixTreeVisitor(pw: java.io.PrintWriter, toPrint: (Show) => String) {
    def visit(show: Show) = {
      pw.println(toPrint(show))
    }
  }
  def traverse(visitor: SuffixTreeVisitor) = {
    def doTraverse(node: BaseNode) {
      node.accept(visitor)
      node match {
        case iNode: InternalNode => 
          for (edge <- iNode.edges.values)
            edge.accept(visitor)
        case _ =>
      }
    }
    doTraverse(root)
  }
  def translateToDot(filename: String) = {
    withPrintWriter(filename) { pw =>
      pw.println("digraph G {\n\trankdir = LR;\n" +
      		"\tedge[arrowsize=0.4, fontsize=10];\n\t" +
      		"node[shape=point];")
      traverse(new SuffixTreeVisitor(pw, (show: Show) =>
        show match {
          case node: BaseNode => node.toDot
          case edge: Edge => edge.toDot
        }))
      pw.println("}")
    }
  }
  def translateToAscii() {
    println(root.toAscii(""))
  }
}

class HTMLSuffixTree(html: Array[String]) extends SuffixTree[String](html, "\0", "$") {
  def findAllRepetitions() = {
    for (key <- root.edges.keys.toList.sortBy(_.filter(_.isDigit))) {
      val minDepth = key.filter(_.isDigit).toInt
      val edge = root.getEdge(key).get
      val edgeSeq = edge.getEdgeSeq
      var rep = edgeSeq.takeWhile(_.filter(_.isDigit).toInt >= minDepth)
      if (rep.length < edgeSeq.length) rep
    } 
  }
}

object TestSuffixTree extends App {
  val s = "dedododeeodo"
  val t1 = new SuffixTree[Char](s, '\0', '$')
  //val html = Array("html1", "head2", "meta3", "meta3", 
  //    "body2", "div3", "a4", "div4", "img5", "div3", "a4", "div4", "img5", "div3")
  val html = Array("body2", "div3", "a4", "div4", "img5", "div3", "a4", "div4", "img5", "div3")
  val head = html(0)
  val son = html(1)
  //val html = Array("body1", "head2", "div3", "a4", "p4", "a4", "p4", "div3", "ul4", "li5", "li5", "li5")
  val t2 = new SuffixTree[String](html, "\0", "$")
  t2.translateToAscii
}