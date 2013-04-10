package thu.ailab.config

import com.twitter.logging.Logger
import com.twitter.logging.FileHandler

/**
 * abstract base class
 */
private[config] abstract class MyConfig

/**
 * Set root logger, and all the sub logger will use the file handler.
 */
private[config] class MyLoggerConfig (val logfile: String) extends MyConfig {
  val logger = Logger.get("")
  logger.clearHandlers
  logger.addHandler(FileHandler(logfile)())
}
