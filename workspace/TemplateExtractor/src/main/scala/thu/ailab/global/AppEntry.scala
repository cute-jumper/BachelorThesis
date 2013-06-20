package thu.ailab.global

import scala.compat.Platform.currentTime

/**
 * Use AppEntry instead of trait App as the program entry
 * because we need to initialize the logger before we run
 * other parts of the program.
 *  
 * See the implementation of trait App for reference.
 */
trait AppEntry extends App {
  LoggerSettings(MyConfigFactory.getValue[String]("logger.filepath"))
}