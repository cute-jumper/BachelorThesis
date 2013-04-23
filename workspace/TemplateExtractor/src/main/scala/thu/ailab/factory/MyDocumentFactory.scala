package thu.ailab.factory

abstract class MyDocumentFactory[T](protected val id2filename: Array[String]) {
  final val size = id2filename.length
  def getFilename(id: Int) = id2filename(id)
  def getInstance(id: Int):T
  protected val documentCache: Array[T]
}

