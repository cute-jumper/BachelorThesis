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
    visitor.tagSeq.toList
  }
  class TagVisitor extends NodeVisitor {
    val tagSeq = new ListBuffer[TreeNode]
    def head(node: Node, depth: Int) = 	{
      node match {
        case n: Element => tagSeq += new TreeNode(node.nodeName(), depth)
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

import thu.ailab.global._
object TestTreeBuilder extends AppEntry {
    import thu.ailab.utils.Tools._
    import java.io.{File, FilenameFilter}
    val MAX_FILE_COUNT = 20
    val filenames = new File("../../Data/blog1000/").listFiles(new FilenameFilter() {
      def accept(dir: File, name: String) = name.endsWith(".html")
    }).slice(0, MAX_FILE_COUNT).map(_.getAbsolutePath)
    val ts = filenames.map {
      new TreeBuilder(_).getTagSequence
    }
    val runner = new thu.ailab.distance.LCSAlgo[TreeNode]
    val results = 
      (for (i <- 0 until ts.length; j <- i + 1 until ts.length) yield {
      (timeIt(runner.run(ts(i), ts(j))), (filenames(i), filenames(j)))
    }).toList
    val (resList, filenamePairList) = results.unzip
    val (sims, times) = resList.unzip
    val averageTime = times.sum / times.length
    println("Average Time: " + averageTime)
    println("Max: " + sims.max + "\nMin: " + sims.min)
    results.foreach(res => logger.info("%s\t%s\n%s", res._2._1, res._2._2, res._1._1))
}