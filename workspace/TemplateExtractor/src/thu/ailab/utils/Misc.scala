package thu.ailab.utils

object Misc {
  def timeIt(method: => Unit) = {
    val start = System.nanoTime()
    method
    (System.nanoTime() - start) / 1e9    
  }
}