package thu.ailab.tree

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.select.NodeVisitor
import org.jsoup.nodes.Node

class TreeBuilder {
  val doc = Jsoup.parse(
      new java.io.File("../../Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00f2e45101017icv.html"), 
      "gbk")
  val elem = doc.getElementsByTag("strong")
//  for (e <- elem.toArray(new Array[Element](0))) println(e)
  def printTree = {
    doc.traverse(new NodeVisitor() {
      def head(node: Node, depth: Int) = println(depth + " Entering " + node.nodeName())
      def tail(node: Node, depth: Int) = println(depth + " Leaving " + node.nodeName())
    })
  }

}

object TestTreeBuilder {
  def main(args: Array[String]) {
    (new TreeBuilder).printTree
  }
}