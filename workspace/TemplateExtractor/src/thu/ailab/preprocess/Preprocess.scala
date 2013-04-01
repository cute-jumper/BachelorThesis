package thu.ailab.preprocess

object Preprocess {
  private val log = com.twitter.logging.Logger.get(getClass)
  def main (args: Array[String]) {
    //val nodes = loadHTML("../../Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00f2e45101017icv.html")
  }
  def loadHTML(filename: String): Option[xml.Elem] = {
    import java.io.{InputStreamReader, FileInputStream}
    import thu.ailab.utils.CharsetDetector
    val charset = CharsetDetector(filename).getOrElse("gbk")
    Some(xml.XML.load(new InputStreamReader(new FileInputStream(filename), charset)))
  }
}

