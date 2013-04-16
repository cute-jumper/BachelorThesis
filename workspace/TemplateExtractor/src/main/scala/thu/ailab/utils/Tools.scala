package thu.ailab.utils

/**
 * Interfaces for the outer to call the internal
 * utilities.
 */

object Tools {
  /**
   * @param process
   * 
   * @return (executionValue, executionTime)
   */
  def timeIt[T](method: => T) = {
    val start = System.nanoTime()
    val ret = method
    (ret, (System.nanoTime() - start) / 1e9)    
  }
  /**
   * @param filename
   * 
   * @return character set's option
   */
  def getFileCharset(filename: String) = CharsetDetector(filename)  
}

