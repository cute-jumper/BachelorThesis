package thu.ailab.document

import java.io.File

import thu.ailab.global.MyConfigFactory
import thu.ailab.tree.{TreeNode, TreeBuilder, HTMLSuffixTree}

class TagSeqFactory(id2filename: Array[String]) 
extends MyDocFactory[Array[TreeNode]](id2filename) {
  override val documentCache = new Array[Array[TreeNode]](size)
  private def fileIsLoaded(id: Int): Boolean = documentCache(id) != null
  implicit def treeNode2String(treeNode: TreeNode) = treeNode.toString
  
  private val blogdir = MyConfigFactory.getValue[String]("document.blogdir")
  private val prepBlogdir = MyConfigFactory.getValue[String]("preprocess.blogdir")
  
  def getPrepFilename(filename: String) = filename.replace(blogdir, prepBlogdir) 
  
  override def getInstance(id: Int) = {
    if (documentCache(id) == null) {
      val filename = id2filename(id)
      val prepFilename = getPrepFilename(filename)
      val tagArray: Array[TreeNode] = {
        if (new File(prepFilename).exists)
          Preprocess.readCompactNodeArray(prepFilename)
        else
          HTMLSuffixTree.stripDuplicates(
              new TreeBuilder(id2filename(id)).getTagSequence.toArray).toArray
      }
      documentCache(id) = tagArray
    }
    documentCache(id)
  }
}
