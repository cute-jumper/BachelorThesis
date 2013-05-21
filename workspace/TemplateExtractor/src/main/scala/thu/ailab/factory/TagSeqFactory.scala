package thu.ailab.factory

import thu.ailab.preprocess.Preprocess
import thu.ailab.tree.{TreeNode, TreeBuilder, HTMLSuffixTree}

class TagSeqFactory(id2filename: Array[String]) 
extends MyDocumentFactory[Array[TreeNode]](id2filename) {
  override val documentCache = new Array[Array[TreeNode]](size)
  private def fileIsLoaded(id: Int): Boolean = documentCache(id) != null
  implicit def treeNode2String(treeNode: TreeNode) = treeNode.toString
  
  private val blogdir = MyConfigFactory.getValue[String]("document.blogdir")
  private val prepBlogdir = MyConfigFactory.getValue[String]("preprocess.blogdir")
  
  def prepExists(filename: String) = { 
    new java.io.File(filename.replace(blogdir, prepBlogdir)).exists
  }
    
  override def getInstance(id: Int) = {
    if (documentCache(id) == null) {
      val filename = id2filename(id)
      val tagArray: Array[TreeNode] = {
        if (prepExists(filename))
          Preprocess.readCompactNodeArray(filename)
        else
          HTMLSuffixTree.stripDuplicates(
              new TreeBuilder(id2filename(id)).getTagSequence.toArray).toArray
      }
      documentCache(id) = tagArray
    }
    documentCache(id)
  }
}
