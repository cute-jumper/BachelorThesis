package thu.ailab.global

import scala.compat.Platform.currentTime

/**
 * See the implementation of trait App for reference
 */
trait AppEntry extends App {
  LoggerSettings(MyConfigFactory.getValue[String]("logger.filepath"))
}