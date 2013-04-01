package thu.ailab.utils

import com.typesafe.config._
import scala.collection.mutable.HashMap
import com.twitter.logging.Logger
import com.twitter.logging.FileHandler
import java.io.File

object MyConfigFactory {
  val conf = ConfigFactory.load()
  private val configCache = new HashMap[String, MyConfig]
  val logfile = conf.getString("logger.filepath")
  configCache("MyLoggerConfig") = new MyLoggerConfig(if (logfile.startsWith("~" + File.separator)) 
    System.getProperty("user.home") + logfile.substring(1)
    else logfile)  
  def get(name: String) = configCache.get(name)
  def main(args: Array[String]) {
  }
}

private[utils] abstract class MyConfig

/**
 * Set root logger, and all the sub logger will use the file handler.
 */
private[utils] class MyLoggerConfig (val logfile: String) extends MyConfig {
  val logger = Logger.get("")
  logger.addHandler(FileHandler(logfile)())
}
