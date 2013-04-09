package thu.ailab.utils

object Misc {
  def timeIt[T](method: => T) = {
    val start = System.nanoTime()
    val ret = method
    (ret, (System.nanoTime() - start) / 1e9)    
  }
}