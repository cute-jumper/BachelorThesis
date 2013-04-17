package thu.ailab.config

import com.typesafe.config._
import scala.collection.mutable.HashMap
import java.io.File

object MyConfigFactory {
  private val conf = ConfigFactory.load()
  private val configCache = new HashMap[String, MyConfig]
  
  val logfile = getConfString(conf, "logger.filepath")
  configCache("MyLoggerConfig") = new MyLoggerConfig(logfile)
  
  val blogdir = getConfString(conf, "document.blogdir")
  val newsdir = getConfString(conf, "document.newsdir")
  configCache("MyFileDirectoriesConfig") = new MyFileDirectoriesConfig(blogdir, newsdir)
  
  val distancesFile = getConfString(conf, "output.distancesFile")
  configCache("MyOutputFilesConfig") = new MyOutputFilesConfig(distancesFile)
  
  val nrOfWorkers = conf.getInt("actor.nrOfWorkers")
  val pieceLength = conf.getInt("actor.pieceLength")
  configCache("MyActorConfig") = new MyActorConfig(nrOfWorkers, pieceLength)
  
  val clusterThreshold = conf.getDouble("cluster.NaiveAggloCluster.clusterThreshold")
  configCache("MyClusterConfig") = new MyClusterConfig(clusterThreshold)
  
  private def getConfString(conf: Config, path: String) = {
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
  /**
   * Get configuration according to the name
   */
  def getConfig(name: String) = configCache.get(name).get
}

