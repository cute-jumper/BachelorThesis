package thu.ailab.tree

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.select.NodeVisitor
import org.jsoup.nodes._
import scala.collection.mutable.ListBuffer
import thu.ailab.preprocess.RawDocument

import thu.ailab.global._
import thu.ailab.distance.LCSArraySpaceOptimized

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
object TestTreeBuilder extends AppEntry with LoggerTrait {
    import thu.ailab.utils.Tools._
    import java.io.{File, FilenameFilter}
  def calcDistance(seq1: Array[TreeNode], seq2: Array[TreeNode]) = {
    new LCSArraySpaceOptimized(seq1, seq2).getDistance()
  }
  def foo1 = {
    val MAX_FILE_COUNT = 100
    val filenames = new File("../../Data/blog1000/").listFiles(new FilenameFilter() {
      def accept(dir: File, name: String) = name.endsWith(".html")
    }).slice(0, MAX_FILE_COUNT).map(_.getAbsolutePath)
    val ts = filenames.map {
      new TreeBuilder(_).getTagSequence.toArray
    }
    val results =
      (for (i <- 0 until ts.length; j <- i + 1 until ts.length) yield {
        (timeIt(calcDistance(ts(i), ts(j))), (filenames(i), filenames(j)))
      }).toList
    val (resList, filenamePairList) = results.unzip
    val (sims, times) = resList.unzip
    val averageTime = times.sum / times.length
    println("Average Time: " + averageTime)
    println("Max: " + sims.max + "\nMin: " + sims.min)
    results.foreach(res => logger.info("%s\t%s\n%s", res._2._1, res._2._2, res._1._1))
  }
  def foo2 = {
    val filenames = List(
      "/home/cutejumper/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00db44d201010rr7.html",
      "/home/cutejumper/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00dcd3260101a7sl.html",
      "/home/cutejumper/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00007e300100f9fq.html",
      "/home/cutejumper/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00dab8c20100g4lp.html",
      "/home/cutejumper/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_000612600100y7nc.html",
      "/home/cutejumper/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00033ac30102e3pz.html",
      "/home/cutejumper/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_002b5d980100szxu.html",
      "/home/cutejumper/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00dab8c20102egxk.html",
      "/home/cutejumper/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_0006126001013no7.html",
      "/home/cutejumper/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00ea674b0100by3m.html",
      "/home/cutejumper/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00084edc0100rj17.html",
      "/home/cutejumper/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00033ac30100q70k.html",
      "/home/cutejumper/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_0000c9a20100bqd6.html",
      "/home/cutejumper/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_002b5d980100smle.html",
      "/home/cutejumper/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fmain_v5%2Fria%2F%2520http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_562792630102dwmp.html")
    val ts = filenames.map {
      new TreeBuilder(_).getTagSequence.toArray
    }
    for (i <- 0 until ts.length; j <- i + 1 until ts.length) {
      println(filenames(i), filenames(j))
      println(timeIt(calcDistance(ts(i), ts(j))))
    }
  }
  def foo3 = {
    val (fn1, fn2) = ("/home/cutejumper/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_000173770100g2g7.html",
    "/home/cutejumper/Programs/BachelorThesis/Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_002b5d980100sf47.html")
    val (t1, t2) = (new TreeBuilder(fn1).getTagSequence.toArray, new TreeBuilder(fn2).getTagSequence.toArray)
    println(t1 mkString " ")
    println(t2 mkString " ")
    println(timeIt(calcDistance(t1, t2)))
  }
  foo3
}