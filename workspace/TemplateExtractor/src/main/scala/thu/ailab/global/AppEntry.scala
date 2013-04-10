package thu.ailab.global

import scala.compat.Platform.currentTime
import thu.ailab.config.MyConfigFactory

/**
 * See the implementation of trait App for reference
 */
trait AppEntry extends LoggerTrait {
  val executionStart: Long = currentTime
  MyConfigFactory
  def main(args: Array[String]) {
    logger.info("App Entry. Execution time: " + (currentTime - executionStart) + "ms")
  }
}