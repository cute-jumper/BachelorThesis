package thu.ailab.preprocess

import java.io.File

import thu.ailab.factory.MyConfigFactory
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
        {for (node <- compactTagSeq) yield node.toXML}
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
  Preprocess.readCompactNodeArray(System.getProperty("user.home") + "/Programs/BachelorThesis/Data/blog10")
}