package thu.ailab.tree

import org.jsoup.Jsoup
import org.jsoup.select.NodeVisitor
import org.jsoup.nodes._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{ HashMap => MHashMap }
import scala.collection.JavaConversions._
import thu.ailab.sequence.TagSequence
import thu.ailab.document.LocalHTMLDocument
import thu.ailab.global._
import thu.ailab.distance.LCSArraySpaceOptimized
import thu.ailab.template.TPTreeNode

/**
 * Traverse the DOM Tree
 */
class TreeBuilder(doc: Document) {
  def this(filename: String) = {
    /**
     * I know this is a rather tricky(or dirty...) way to overcome
     * the restriction of Scala's rules for auxiliary constructor...
     * Any new awesome ideas?
     */
    this(((filename: String) => {
      val htmlDoc = new LocalHTMLDocument(filename)
      Jsoup.parse(htmlDoc.simplifiedContent, htmlDoc.charset)
    })(filename))
  }
  def printTree = {
    doc.traverse(new NodeVisitor() {
      def head(node: Node, depth: Int) = println(depth + " Entering " + node.nodeName())
      def tail(node: Node, depth: Int) = println(depth + " Leaving " + node.nodeName())
    })
  }
  /**
   * Functions using "Visitor Pattern"!!
   */
  def getTagSequence() = {
    val visitor = new TagSeqVisitor
    doc.traverse(visitor)
    visitor.tagSeq
  }
  def getVerboseTagSequence() = {
    val visitor = new VerboseTagSeqVisitor
    doc.traverse(visitor)
    visitor.verboseTagSeq
  }
  def getTree() = {
    val visitor = new TPTreeVisitor
    doc.children().head.traverse(visitor)
    visitor.root
  }
  /**
   * Visitor for TreeNode
   */
  class TagSeqVisitor extends NodeVisitor {
    val tagSeq = new ArrayBuffer[TreeNode]
    def head(node: Node, depth: Int) = {
      node match {
        case n: Element => tagSeq += new TreeNode(node.nodeName(), depth)
        case n: TextNode => //tagSeq += new TreeNode("text: " + n.text(), depth)
        case n: DataNode => //tagSeq += new TreeNode("data: " + n.getWholeData(), depth)
        case _ =>
      }
    }
    def tail(node: Node, depth: Int) = {
    }
  }
  /**
   * Visitor for VerboseTreeNode
   */
  class VerboseTagSeqVisitor extends NodeVisitor {
    val verboseTagSeq = new ArrayBuffer[VerboseTreeNode]
    def head(node: Node, depth: Int) = {
      node match {
        case n: Element => verboseTagSeq += new VerboseTreeNode(node.nodeName(), depth, n)
        case n: TextNode => //tagSeq += new TreeNode("text: " + n.text(), depth)
        case n: DataNode => //tagSeq += new TreeNode("data: " + n.getWholeData(), depth)
        case _ =>
      }
    }
    def tail(node: Node, depth: Int) = {
    }
  }
  /**
   * Visitor for TPTreeNode
   */
  class TPTreeVisitor extends NodeVisitor {
    val root = new TPTreeNode(new TreeNode(doc.nodeName, 0), None)
    val jsoupToTP = new MHashMap[Node, TPTreeNode]
    jsoupToTP += (doc -> root)
    def head(node: Node, depth: Int) = {
      val name = node match {
        case n: Element =>
          n.nodeName()
          val tn = new TreeNode(n.nodeName, depth + 1)
          val father = jsoupToTP(node.parent())
          val tpTreeNode = new TPTreeNode(tn, Some(father))
          father.addChild(tpTreeNode)
          jsoupToTP += node -> tpTreeNode
        case n: TextNode => //n.getWholeText().trim()
        case n: DataNode => //n.getWholeData().trim()
        case _ => //"ERROR!!!"
      }
    }
    def tail(node: Node, depth: Int) = {
    }
  }
}

object TestTreeBuilder extends App {
  val fn = sys.props("user.home") + "/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00f2e45101017088.html"
  val vTagSeq = new TreeBuilder(fn).getVerboseTagSequence.toArray
  val compact = HTMLSuffixTree.stripDuplicates(vTagSeq)
  for (vnode <- compact if vnode.allowMultiple) {
    println("verbose node: " + vnode.toString)
    println(vnode.relatedRoots.map(_.nodeName()).mkString(" | "))
  }
}