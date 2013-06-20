package thu.ailab.utils

import thu.ailab.global.MyConfigFactory
import thu.ailab.global.LoggerTrait

/**
 * Interfaces for the outer to call the internal
 * utilities.
 */

object Tools extends LoggerTrait {
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
   * Define a new control structure to use PrintWriter
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
  import java.io.File
  val dataset = MyConfigFactory.getValue[String]("global.dataset")  
  val trainsize = MyConfigFactory.getValue[Int](dataset, "trainsize")
  val docDir = MyConfigFactory.getValue[String](dataset, "document.dir")
  /**
   * Helper functions to get files according to the configuration.
   */
  def getTrainFiles() = {
    new File(docDir).listFiles().slice(0, trainsize).map(_.getAbsolutePath)
  }
  def getTestFiles() = {
    val files = new File(docDir).listFiles()
    files.slice(trainsize, files.size).map(_.getAbsolutePath)
  }
  import util.Random
  def getRandomTestFile() = {
    val files = new File(docDir).listFiles()
    val testsize = files.size - trainsize
    val chosen = files(Random.nextInt(testsize) + trainsize).getAbsolutePath
    logger.info("random choose: " + chosen)
    chosen
  }
}