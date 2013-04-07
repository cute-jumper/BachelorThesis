package thu.ailab.tree

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.select.NodeVisitor
import org.jsoup.nodes._
import scala.collection.mutable.ListBuffer
import thu.ailab.preprocess.RawDocument

class TreeBuilder(filename: String) {
  val rawDoc = new RawDocument(filename)
  val doc = Jsoup.parse(rawDoc.simplifiedContent, rawDoc.charset)
  val elem = doc.getElementsByTag("strong")
  def printTree = {
    doc.traverse(new NodeVisitor() {
      def head(node: Node, depth: Int) = println(depth + " Entering " + node.nodeName())
      def tail(node: Node, depth: Int) = println(depth + " Leaving " + node.nodeName())
    })
  }
  def getTagSequence = {
    val visitor = new TagVisitor
    doc.traverse(visitor)
    visitor.tagSeq
  }
  class TagVisitor extends NodeVisitor {
    val tagSeq = new ListBuffer[TreeNode]
    def head(node: Node, depth: Int) = 	{
      node match {
        case n: Element => tagSeq += new TreeNode("<" + node.nodeName() + ">", depth)
        case n: TextNode => //tagSeq += new TreeNode("text: " + n.text(), depth)
        case n: DataNode => //tagSeq += new TreeNode("data: " + n.getWholeData(), depth)
        case _ =>
      }
    }
    def tail(node: Node, depth: Int) = {
      //tagSeq += new TreeNode("</" + node.nodeName() + ">", depth)
      node match {
        case n: Element => tagSeq += new TreeNode("</" + node.nodeName() + ">", depth)
        case n: TextNode => //tagSeq += new TreeNode("text: " + n.text(), depth)
        case n: DataNode => //tagSeq += new TreeNode("data: " + n.getWholeData(), depth)
        case _ =>
      }
    }
  }
}

object TestTreeBuilder {
  def main(args: Array[String]) {
    import thu.ailab.utils.Misc._
    val t1 = new TreeBuilder("../../Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00f2e45101017icv.html")
    val t2 = new TreeBuilder("../../Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00f2e45101017icz.html")
    val t3 = new TreeBuilder("../../Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00f1d6140100j68c.html")
    val runner = new thu.ailab.algo.LCSAlgo[TreeNode]    
    println(timeIt(println(runner.run(t1.getTagSequence.toList, t2.getTagSequence.toList))))
  }
}