package thu.ailab.preprocess

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element


object Preprocess {
  import com.typesafe.config._
  val doc = Jsoup.parse(
      new java.io.File("../../Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00f2e45101017icv.html"), "gbk")
  val elem = doc.getElementsByTag("strong")
  for (e <- elem.toArray(new Array[Element](0))) println(e)
  private val log = com.twitter.logging.Logger.get(getClass)

  //log.addHandler(FileHandler(path)())
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

