package thu.ailab.tree

abstract class MyFactory[T] {
  val size: Int
  def getInstance(id: Int):T
  protected val cache: Array[T]
}

class TagSeqFactory(dir: String) extends MyFactory[List[TreeNode]] {
  private val id2filename = new java.io.File(dir).listFiles.map(_.getAbsolutePath)
  override val size = id2filename.length
  
  override val cache = new Array[List[TreeNode]](size)
  private def fileIsLoaded(id: Int): Boolean = cache(id) != null
  
  override def getInstance(id: Int) = {
    if (cache(id) == null)
      cache(id) = new TreeBuilder(id2filename(id)).getTagSequence
    cache(id)
  }
}
