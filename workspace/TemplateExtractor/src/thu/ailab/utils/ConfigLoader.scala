package thu.ailab.utils

object ConfigLoader {
  private final val configFilePath = getClass.getResource("/conf/config.conf").getPath
  println(configFilePath)
  new java.io.File(configFilePath).listFiles.foreach(println)
  val config = xml.XML.loadFile(configFilePath)
  new ConfigLoader((config \\ "logging" \ "filepath").text)
  def main(args: Array[String]) {
    ConfigLoader
  }
}

case class ConfigLoader(val loggingFile: String)