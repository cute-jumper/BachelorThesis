package thu.ailab.preprocess


object Preprocess {
  import com.twitter.logging._
  private val log = Logger.get(getClass)
  println(getClass.getResource("/").getPath())
  
  //log.addHandler(FileHandler(path)())
  def main (args: Array[String]) {
    val nodes = loadHTML("../../Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00f2e45101017icv.html")
    log.info("Hello World")
  }
  def loadHTML(fn: String): Option[xml.Elem] = {
    import java.io.{InputStreamReader, FileInputStream}
    import thu.ailab.utils.{CharsetDetector, CharsetNotConfidentException}
    try {
      val charset = CharsetDetector(fn)
      Some(xml.XML.load(new InputStreamReader(new FileInputStream(fn), charset)))
    } catch {
      case e: CharsetNotConfidentException => 
        log.error("Poor confidence of " + e.charset + ": " + e.confidence)
        None
      case e => log.error("Exception Happened!");throw e
    }
  }
  import java.net.URLDecoder.decode
  
  def removeBlank(input: Array[String]) = {
    input.filter(_.length != 0).map(_.trim)
  }
  
  def removeTags(tags: Array[String]) = {
    for (tag <- tags) {
      
    }
  }
}

