package thu.ailab.utils

import com.typesafe.config._
import scala.collection.mutable.HashMap
import com.twitter.logging.Logger
import com.twitter.logging.FileHandler

object MyConfigFactory {
  val conf = ConfigFactory.load()  
  private val configCache = new HashMap[String, MyConfig]
  configCache("MyLoggerConfig") = new MyLoggerConfig(conf.getString("logger.filepath"))
  
  def get(name: String) = configCache.get(name)
  def main(args: Array[String]) {
  }
}

private[utils] abstract class MyConfig

private[utils] class MyLoggerConfig (val logfile: String) extends MyConfig {
  val logger = Logger.get("")
  logger.addHandler(FileHandler(logfile)())
}
