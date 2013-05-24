package thu.ailab.document

import java.io.File

import thu.ailab.global.MyConfigFactory
import thu.ailab.tree.{TreeNode, TreeBuilder, HTMLSuffixTree}
import thu.ailab.utils.Tools.withPrintWriter

object Preprocess {
  def HTMLToCompactNodeArray() = {
    val blogdir = MyConfigFactory.getValue[String]("document.blogdir")
    val blogFilenames = new File(blogdir).listFiles().map(_.getAbsolutePath())
    val prepBlogdir = MyConfigFactory.getValue[String]("preprocess.blogdir")
    val f = new File(prepBlogdir)
    if (!f.exists()) {
      f.mkdir()
    }
    for (fn <- blogFilenames) {
      val compactTagSeq = HTMLSuffixTree.stripDuplicates(
          new TreeBuilder(fn).getTagSequence.toArray)
      val node = 
        <TreeNodeArray>
        {compactTagSeq.map(_.toXML)}
        </TreeNodeArray>
      xml.XML.save(fn.replace(blogdir, prepBlogdir), node)
    }
  }
  def readCompactNodeArray(filename: String) = {
    val nodeArrayXML = xml.XML.loadFile(filename)
    val treeNodeArray = (nodeArrayXML \ "TreeNode" map { n =>
      val nameArray = (n \ "names" \ "name" map (_.text)).toArray
      val depthArray = (n \ "depths" \ "depth" map (_.text.toInt)).toArray
      val allowMultiple = n.attribute("allowMultiple").get.text.toBoolean
      new TreeNode(nameArray, depthArray, allowMultiple)
    })
    treeNodeArray.toArray
  }
}

object TestPreprocess extends App {
//  val nodeArray = Preprocess.readCompactNodeArray(System.getProperty("user.home") + 
//      "/Programs/BachelorThesis/Data/blog1000_preprocess/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00f2e45101017a26.html")
//  println(nodeArray.mkString(" "))
//  val prepBlogdir = MyConfigFactory.getValue[String]("preprocess.blogdir")
//  for (filename <- new File(prepBlogdir).listFiles().map(_.getAbsolutePath)) {
//    Preprocess.readCompactNodeArray(filename)
//  }
  Preprocess.HTMLToCompactNodeArray
}