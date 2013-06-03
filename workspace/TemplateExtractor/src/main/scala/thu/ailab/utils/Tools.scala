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
   * new control structure to use PrintWriter
   */
  import java.io.PrintWriter
  def withPrintWriter(filename: String)(op: PrintWriter => Unit) {
    val writer = new PrintWriter(filename)
    try {
      op(writer)
    } finally {
      writer.close()
    }
  }
}

