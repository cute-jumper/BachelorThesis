package thu.ailab.global

import scala.compat.Platform.currentTime
import thu.ailab.factory.MyConfigFactory

/**
 * See the implementation of trait App for reference
 */
trait AppEntry extends App {
  LoggerSettings(MyConfigFactory.getValue[String]("logger.filepath"))
}