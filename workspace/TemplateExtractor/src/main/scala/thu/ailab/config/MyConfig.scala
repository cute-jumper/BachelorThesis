package thu.ailab.config

import com.twitter.logging.Logger
import com.twitter.logging.FileHandler

/**
 * abstract base class
 */
private[config] abstract class MyConfig {
  val valueCache: Map[String, String]
  def getValue(name: String): String = valueCache.get(name).get
}

/**
 * Set root logger, and all the sub logger will use the file handler.
 */
class MyLoggerConfig (val logfile: String) extends MyConfig {
  val logger = Logger.get("")
  logger.clearHandlers
  logger.addHandler(FileHandler(logfile)())
  logger.setLevel(com.twitter.logging.Level.INFO)
  override val valueCache = Map("logfile" -> logfile)
}

class MyFileDirectoriesConfig (val blogdir: String, val newsdir: String) 
extends MyConfig {
  override val valueCache = Map("blogdir" -> blogdir, "newsdir" -> newsdir)
}

class MyOutputFilesConfig (val distancesFile: String) extends MyConfig {
  override val valueCache = Map("distancesFile" -> distancesFile)
}

class MyActorConfig (val nrOfWorkers: Int, val pieceLength: Int) extends MyConfig {
  override val valueCache = Map("nrOfWorkers" -> nrOfWorkers.toString,
      "pieceLength" -> pieceLength.toString) 
}

class MyClusterConfig (val clusterThreshold: Double) extends MyConfig {
  override val valueCache = Map("clusterThreshold" -> clusterThreshold.toString)
}