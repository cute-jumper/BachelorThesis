package thu.ailab.template

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

import thu.ailab.tree.TreeNode
import thu.ailab.utils.RenderChars

class TPTreeNode(val treeNode: TreeNode, val father: Option[TPTreeNode]) {
  val depth = treeNode.depth
  val children = new ArrayBuffer[TPTreeNode]
  def addChild(tpTreeNode: TPTreeNode) = {
    children += tpTreeNode
  }
  private val EDGE_LINE = RenderChars.HorizontalLine * 3
  private val EDGE_BLANK = " " * EDGE_LINE.length()
  def toASCII(prefix: String): String = {
    val nodeLabel = treeNode.toString
    val prefixPadding = prefix + " " * nodeLabel.length
    val nextPrefix =  prefixPadding + "%s " + EDGE_BLANK
    nodeLabel +
      (if (children.size == 0) {
        ""
      } else if (children.size == 1) {
        RenderChars.HorizontalLine * 2 + EDGE_LINE +
          children.head.toASCII(nextPrefix.format(" "))
      } else {
        (for ((child, idx) <- children.zipWithIndex) yield {
          if (idx == 0) {
            RenderChars.TJunctionDown + RenderChars.HorizontalLine + EDGE_LINE +
              child.toASCII(nextPrefix.format(RenderChars.VerticalLine))
          } else if (idx == children.size - 1) {
            prefixPadding + RenderChars.CornerRight + RenderChars.HorizontalLine + EDGE_LINE +
              child.toASCII(nextPrefix.format(" "))
          } else {
            prefixPadding + RenderChars.TJunctionRight + RenderChars.HorizontalLine + EDGE_LINE +
              child.toASCII(nextPrefix.format(RenderChars.VerticalLine))
          }
        }).mkString("\n")
      })
  }
}

object TPTreeNode {
  def makeTPTree(tnArray: Array[TreeNode], posToOpNode: Map[Int, OptionalNode]) = {
    @tailrec
    def getAncestor(curNode: TPTreeNode, step: Int): TPTreeNode = {
      if (step == 0) curNode
      else getAncestor(curNode.father.get, step - 1)
    }
    val root = new TPTreeNode(tnArray.head, None)
    var curNode = root
    for ((tn, idx) <- tnArray.tail.zip (1 until tnArray.length)) {
      val father =
        if (tn.depth <= curNode.depth) getAncestor(curNode, curNode.depth - tn.depth + 1)
        else curNode
      val child = new TPTreeNode(tn, Some(father))
      father.addChild(child)
      curNode = child
      if (posToOpNode.contains(idx)) {
        val opNode = posToOpNode(idx)
      }
    }
    root
  }
}