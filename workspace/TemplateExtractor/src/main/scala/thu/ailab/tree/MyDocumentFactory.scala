package thu.ailab.tree

abstract class MyDocumentFactory[T](dir: String) {
  protected val id2filename = new java.io.File(dir).listFiles.map(_.getAbsolutePath)
  final val size = id2filename.length
  def getFilename(id: Int) = id2filename(id)
  def getInstance(id: Int):T
  protected val documentCache: Array[T]
}

class TagSeqFactory(dir: String) extends MyDocumentFactory[List[TreeNode]](dir) {
  override val documentCache = new Array[List[TreeNode]](size)
  private def fileIsLoaded(id: Int): Boolean = documentCache(id) != null
  
  override def getInstance(id: Int) = {
    if (documentCache(id) == null)
      documentCache(id) = new TreeBuilder(id2filename(id)).getTagSequence
    documentCache(id)
  }
}
