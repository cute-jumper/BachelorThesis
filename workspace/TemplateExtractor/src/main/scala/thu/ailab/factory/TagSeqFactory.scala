package thu.ailab.factory

import thu.ailab.tree.{TreeNode, TreeBuilder}

class TagSeqFactory(id2filename: Array[String]) extends MyDocumentFactory[Array[TreeNode]](id2filename) {
  override val documentCache = new Array[Array[TreeNode]](size)
  private def fileIsLoaded(id: Int): Boolean = documentCache(id) != null
  
  override def getInstance(id: Int) = {
    if (documentCache(id) == null) {
      val tagArray: Array[TreeNode] = new TreeBuilder(id2filename(id)).getTagSequence.toArray
      documentCache(id) = tagArray
    }
    documentCache(id)
  }
}
