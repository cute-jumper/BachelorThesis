package thu.ailab.document

abstract class MyDocFactory[T](protected val id2filename: Array[String]) {
  final protected val factorySize = id2filename.length
  def getSize() = factorySize
  def getFilename(id: Int) = id2filename(id)
  def getInstance(id: Int):T
  protected val documentCache: Array[T]
}

