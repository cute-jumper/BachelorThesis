package thu.ailab.global

import scala.compat.Platform.currentTime
import thu.ailab.config.MyConfigFactory

/**
 * See the implementation of trait App for reference
 */
trait AppEntry extends App {
  LoggerSettings(MyConfigFactory.getValue[String]("logger.filepath"))
}