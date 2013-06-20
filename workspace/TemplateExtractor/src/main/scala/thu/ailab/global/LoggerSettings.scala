package thu.ailab.global

import com.twitter.logging.Logger
import com.twitter.logging.FileHandler

/**
 * Set root logger, and all the sub logger will use the file handler.
 * In order to call this at startup, programs should extend the AppEntry.
 */
object LoggerSettings {
  def apply(logfile: String) {
    val logger = Logger.get("")
    logger.clearHandlers
    logger.addHandler(FileHandler(logfile)())
    logger.setLevel(com.twitter.logging.Level.INFO)
  }
}
