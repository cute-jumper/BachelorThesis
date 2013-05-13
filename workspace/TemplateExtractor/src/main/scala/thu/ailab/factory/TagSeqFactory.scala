package thu.ailab.factory

import thu.ailab.tree.{TreeNode, TreeBuilder, HTMLSuffixTree}

class TagSeqFactory(id2filename: Array[String]) 
extends MyDocumentFactory[Array[TreeNode]](id2filename) {
  override val documentCache = new Array[Array[TreeNode]](size)
  private def fileIsLoaded(id: Int): Boolean = documentCache(id) != null
  implicit def treeNode2String(treeNode: TreeNode) = treeNode.toString 
  
  override def getInstance(id: Int) = {
    if (documentCache(id) == null) {
      val tagArray: Array[TreeNode] = HTMLSuffixTree.stripRepetitions( 
          new TreeBuilder(id2filename(id)).getTagSequence.toArray).toArray
      documentCache(id) = tagArray
    }
    documentCache(id)
  }
}
