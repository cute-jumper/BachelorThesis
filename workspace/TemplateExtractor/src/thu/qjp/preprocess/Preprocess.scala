package thu.qjp.preprocess

object Preprocess {
  def main (args: Array[String]) {
  	test
  }
  def loadHTML(fn: String) = {
    import java.io.{BufferedInputStream, FileInputStream}
  	import com.ibm.icu.text.{CharsetMatch, CharsetDetector}
    val detector = new CharsetDetector
  	detector.setText(new )
  }
  def test = {
    val fn = "../../Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00f2e45101017icv.html"
    detector.setText(new java.io.BufferedInputStream(new java.io.FileInputStream(fn)))
    val res = detector.detect
    println(res.getName())
    println(res.getLanguage())
    println(res.getConfidence())
    val nodes = xml.XML.load(new java.io.InputStreamReader(new java.io.FileInputStream(fn), res.getName))
    println(nodes.toString)
    
  }	
  
  import java.io.File
  import java.net.URLDecoder.decode
  
  def stripBlank(input: Array[String]) = {
    input.filter(_.length != 0).map(_.trim)
  }
  
  def stripTags(tags: Array[String]) = {
    for (tag <- tags) {
      
    }
  }
}