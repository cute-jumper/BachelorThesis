package thu.ailab.tree

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

/**
 * This is an implementation of the Suffix Tree in order to find out the
 * repeated data record in HTML source file.
 * 
 * Note that I wrote this implementation according to the SO question:
 * http://stackoverflow.com/questions/9452701/ukkonens-suffix-tree-algorithm-in-plain-english
 * 
 * There two ways to display the result tree:
 * 1. toAscii: an ASCII tree
 * 2. toDot: translate to a dot file. Use `dot -Tpng input.dot -o output.png'
 *    to generate a PNG image.  
 */
class SuffixTree[T : Ordering](rawInputSeq: IndexedSeq[T], 
    val nullEdge: T, 
    val canonicalEnd: T,
    val verbose: Boolean = true) {
  /**
   * Add `canonicalEnd' to the string if the last element
   * occurs in the subsequence before
   */
  val inputSeq =
    if (rawInputSeq.indexOf(
        rawInputSeq(rawInputSeq.length - 1)) < rawInputSeq.length - 1)
      rawInputSeq :+ canonicalEnd
    else rawInputSeq
    
  def getInputSeqSlice(beginIndex: Int, endIndex: Int) = {
    inputSeq.slice(beginIndex, endIndex)
  } 
  
  /**
   * Base class for the Edge and Node in order to implement the
   * `Visitor Pattern'
   */ 
  abstract class Show {
    def accept(visitor: SuffixTreeVisitor)
  }
  class Edge(val parentNode: InternalNode, val beginIndex: Int) extends Show {
    /**
     *  Auxiliary construct
     */ 
    def this(_parentNode: InternalNode, _beginIndex: Int, _endNode: BaseNode, 
        _fixEndIndex: Int) = {
      this(_parentNode, _beginIndex)
      this.endNode = _endNode
      this.fixEndIndex = _fixEndIndex
    }
    // Used when the edge is closed
    private var fixEndIndex: Int = 0
    var endNode: BaseNode = Leaf()
    val ranges = new ArrayBuffer[Int]
    /**
     * Link to a InternalNode or a Leaf, 
     * or whether this edge is closed
     */
    def isClosed = !endNode.isLeaf
    /**
     * Various getters for internal use
     */
    def getEndIndex() = if (isClosed) fixEndIndex else endPtr + 1
    def getElemAt(idx: Int) = inputSeq(beginIndex + idx)
    def getEdgeSeq = inputSeq.slice(beginIndex, getEndIndex)
    def getEdgeSeqLength = getEndIndex - beginIndex
    
    /**
     * Important function used in construction
     */
    def splitEdge(edgeLen: Int, _beginIndex: Int) = {
      val iNode = InternalNode()
      // save original state before any modification
      val save = sameExceptStart(iNode, beginIndex + edgeLen)
      fixEndIndex = beginIndex + edgeLen
      iNode.addEdge(_beginIndex)
      iNode.addEdge(inputSeq(fixEndIndex), save)
      endNode = iNode
    }
    private def sameExceptStart(_parentNode: InternalNode, _beginIndex: Int) = {
      new Edge(_parentNode, _beginIndex, this.endNode, this.fixEndIndex)
    }
    
    /**
     * For display purposes
     */    
    def getEdgeString = inputSeq.slice(beginIndex, getEndIndex).mkString(" ")
    def getEdgeStringLength = getEdgeString.length
    
    def toAscii(prefix: String, maxEdgeLength: Int) = {
      getEdgeString + (endNode match {
        case node: InternalNode =>
          RenderChars.HorizontalLine * (maxEdgeLength - getEdgeStringLength + 1) + 
          node.toAscii(prefix + " " * (maxEdgeLength + 1))
        case _ => ""
      })
    }
    def toDot() = {
      val compactString = getEdgeString split " " map { s =>
        "%c%c".format(s(0), s(s.length - 1))
      } mkString " "
      "\t" + parentNode.name + "->" + endNode.name + 
      "[label=\"%s\"];".format(compactString)
    }
    def accept(visitor: SuffixTreeVisitor) = {
      visitor.visit(Edge.this)
    }
  }
  /**
   * Base class for InternalNode and Leaf
   */
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
  /**
   * Return two closures for the subclass object.
   * The first one will be used to construct subclass
   * object and the second is just a getter.
   */
  private def InstanceBuilder[T](stm: (Int) => T) = {
    var number = -1
    (() => {
      number += 1
      stm(number)
    }, () => number)
  }
  object Leaf {
    val (getInstance, getNumber) = InstanceBuilder(new Leaf(_))
    def apply() = getInstance()
    def getTotalLeafCount = getNumber() + 1
  }
  object InternalNode {
    val (getInstance, getNumber) = InstanceBuilder(new InternalNode(_))
    def apply() = getInstance()
    def getTotalInternalNodeCount = getNumber() + 1
  }
  class Leaf private(number: Int) extends BaseNode("leaf", number) {
    val isLeaf = true
  }  
  class InternalNode private(number: Int) extends BaseNode("node", number) {
    val edges = new MHashMap[T, Edge]
    var suffixLink: Option[InternalNode] = None
    var childLeafCount = 0
    val isLeaf = false
    
    def hasEdge(c: T) = edges.contains(c)
    def addEdge(idx: Int) = {
      edges(inputSeq(idx)) = new Edge(InternalNode.this, idx)
    }
    def addEdge(c: T, edge: Edge) = {
      edges(c) = edge
    }
    def getEdge(c: T) = edges.get(c)
    def getNodeLabel = "(" + name.filter(_.isDigit) + ")"
    
    /**
     * For display purposes
     */
    def toAscii(prefix: String): String = {
      val nodeLabel = getNodeLabel + (
          if (suffixLink.isDefined) "->" + suffixLink.get.getNodeLabel 
          else "")
      val maxEdgeLength = edges.values.maxBy(_.getEdgeStringLength).getEdgeStringLength
      val prefixPadding = prefix + " " * nodeLabel.length
      nodeLabel + (if (edges.size == 1) {
        RenderChars.HorizontalLine * 2 +
        edges.values.headOption.get.toAscii(prefixPadding + "  ", maxEdgeLength)
      }
      else {
        (for ((edge, idx) <- edges.values.toList.sortBy(
            edge => -edge.getEdgeSeqLength).zipWithIndex) yield {
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
  /**
   * Tree variables
   * 1. endPtr: points to the current last position in the input sequence
   * 2. root: stands for the tree's root
   * 3. activePoint: a triple tuple indicating where the insertion should 
   *    happen
   * 4. remainder: indicates how many elements left in the sequence that wait
   *    for insertion.
   */
  var endPtr = 0
  val root = InternalNode()
  val activePoint = new ActivePoint
  var remainder: Int = 1
  
  class ActivePoint {
    var activeNode = root
    var activeEdge = nullEdge
    var activeLength = 0
    /**
     * See if a new element should be inserted into the tree.
     */
    def tryInsert(c: T) = {
      var ret = false
      if (activeEdge == nullEdge && activeNode.hasEdge(c)) {
        activeEdge = c
        activeLength += 1
      } else {
        if (activeNode.hasEdge(activeEdge) &&
          activeNode.getEdge(activeEdge).get.getElemAt(activeLength) == c) {
          activeLength += 1
        } else {
          ret = true
        }
      }
      normalizeAfterUpdate
      ret
    }
    /**
     * Perform an insertion from the active point.
     * Return the new edge's endNode
     */
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
    /**
     * Update the active point after an insertion.
     * Add a new parameter `endNode' according to the SO question's second
     * answer.
     */
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
    /**
     * Normalize when a new element is not really inserted to the tree.
     * Note we call this function every time we call `tryInsert', meaning
     * that this function is not necessary to be recursive.  
     */
    def normalizeAfterUpdate() = {
      if (activeNode.hasEdge(activeEdge)) {
        val edge = activeNode.getEdge(activeEdge).get
        if (activeLength == edge.getEdgeSeqLength && edge.isClosed) {
          activeNode = edge.endNode.asInstanceOf[InternalNode]
          activeEdge = nullEdge
          activeLength = 0
        }
      }
    }
    /**
     * Normalize when we move the active point along with the suffix link.
     */
    def normalizeAfterSuffix(oldEdge: Edge, initialIndex: Int) = {
      @tailrec
      def doNormalize(beginIndex: Int): Unit = {
        if (activeNode.hasEdge(activeEdge)) {
          val newEdge = activeNode.getEdge(activeEdge).get
          if (newEdge.getEdgeSeqLength <= activeLength) {
            assert(!newEdge.endNode.isLeaf)
            activeNode = newEdge.endNode.asInstanceOf[InternalNode]
            activeLength -= newEdge.getEdgeSeqLength
            if (activeLength == 0) activeEdge = nullEdge
            else activeEdge = oldEdge.getElemAt(beginIndex + newEdge.getEdgeSeqLength)
            doNormalize(beginIndex + newEdge.getEdgeSeqLength)
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
  /**
   * Top-level insertion function
   * 
   * @param times how many insertion should we make
   * @param preInsertNode node previous inserted
   */
  @tailrec
  private def insertSuffix(times: Int, 
      preInsertNode: Option[InternalNode]): Unit = {
    def doInsert = {
      val c = inputSeq(endPtr)
      if (!activePoint.tryInsert(c)) {
        // Need not to insert. Increase the remainder
        remainder += 1
        None
      } else {
        // do real insertion
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
        case _ => // if we did not insert a node at previous step, stop recursion
      }
    }
  }
  def build {
    if (endPtr < inputSeq.length) {
      if (verbose) {
        println(activePoint)
        println("# remainder: " + remainder)
      }
      insertSuffix(remainder, None)
      if (verbose) {
        println("insert %s at %d".format(inputSeq(endPtr), endPtr))
        translateToAscii
      }
      endPtr += 1
      build
    } else {
      endPtr -= 1 // End of building, making endPtr point to right place
    }
  }
  /**
   * The build process of the main constructor
   */
  build
  postBuild  
  assert(inputSeq.length == root.childLeafCount)
  
  def postBuild {
    def updateNodeEdge(node: InternalNode) {      
      for (edge <- node.edges.values) {
        if (edge.isClosed) {
          val nextNode = edge.endNode.asInstanceOf[InternalNode]
          updateNodeEdge(nextNode)
          for (childEdge <- nextNode.edges.values) {          
            edge.ranges ++= childEdge.ranges.map(_ - edge.getEdgeSeqLength)
          }
          node.childLeafCount += nextNode.childLeafCount
        } else {
          edge.ranges += edge.beginIndex
          node.childLeafCount += 1
        }
      }
    }
    updateNodeEdge(root)
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
          for (edge <- iNode.edges.values) {
            edge.accept(visitor)
            doTraverse(edge.endNode)
          }
        case _ =>
      }
    }
    doTraverse(root)
  }
  /**
   * Store the suffix tree in graphviz dot format
   */
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