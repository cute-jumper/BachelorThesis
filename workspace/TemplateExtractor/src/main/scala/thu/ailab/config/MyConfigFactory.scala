package thu.ailab.config

import com.typesafe.config._
import scala.collection.mutable.HashMap
import java.io.File

object MyConfigFactory {
  private val conf = ConfigFactory.load()
  def getConfString(path: String) = {
    /**
     * Replace the `~' with user's home directory
     */
    def simpleExpansion(filepath: String) = 
      if (filepath.startsWith("~" + File.separator)) 
        System.getProperty("user.home") + filepath.substring(1)
      else 
        filepath
    simpleExpansion(conf.getString(path))
  }
  def getConfDouble(path: String) = conf.getDouble(path)
  def getConfInt(path: String) = conf.getInt(path)
}

