package thu.ailab.tree

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.select.NodeVisitor
import org.jsoup.nodes._
import scala.collection.mutable.ListBuffer
import thu.ailab.preprocess.RawDocument
import thu.ailab.utils.MyConfigFactory

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
        case n: Element => //tagSeq += new TreeNode("</" + node.nodeName() + ">", depth)
        case n: TextNode => //tagSeq += new TreeNode("text: " + n.text(), depth)
        case n: DataNode => //tagSeq += new TreeNode("data: " + n.getWholeData(), depth)
        case _ =>
      }
    }
  }
}

object TestTreeBuilder {
  MyConfigFactory
  val logger = com.twitter.logging.Logger.get(getClass)
  def main(args: Array[String]) {
    import thu.ailab.utils.Misc._
    import java.io.{File, FilenameFilter}
    val MAX_FILE_COUNT = 10
    val filenames = new File("../../Data/blog1000/").listFiles(new FilenameFilter() {
      def accept(dir: File, name: String) = name.endsWith(".html")
    }).slice(0, MAX_FILE_COUNT).map(_.getAbsolutePath)
//    val filenames = List("../../Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00f2e45101017icv.html",
//        "../../Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00f2e45101017icz.html",
//        "../../Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00f1d6140100j68c.html")
    val ts = filenames.map {
      new TreeBuilder(_).getTagSequence.toList
    }
    val runner = new thu.ailab.algo.LCSAlgo[TreeNode]
    val (sims, times) = (for (i <- 0 until ts.length; j <- i + 1 until ts.length) yield {
      timeIt(runner.run(ts(i), ts(j)))
    }).toList.unzip
    val averageTime = times.sum / times.length
    println("Average Time: " + averageTime)
    println("Max: " + sims.max + "\nMin: " + sims.min)
    sims.foreach(logger.info("%s", _))
  }
}