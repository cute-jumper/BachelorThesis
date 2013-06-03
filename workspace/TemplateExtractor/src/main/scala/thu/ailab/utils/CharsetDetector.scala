package thu.ailab.utils

import com.twitter.logging.Logger
import java.io.{BufferedInputStream, FileInputStream}
import com.ibm.icu.text.{CharsetMatch, CharsetDetector}

/**
 * Simple wrapper of icu4j, which can used to automatically detect
 * the character set of the HTML documents.
 * 
 * @param filname
 * Path to the file
 * 
 * @return Option[String]
 * Possible character set with confidence not below than 50, wrapped
 * in an option type.
 */
object MyCharsetDetector {
  private val logger = Logger.get(getClass)
  val detector = new CharsetDetector
  def detectFile(filename: String): Option[String] = {
    val buf = new BufferedInputStream(new FileInputStream(filename))
    detector.setText(buf)
    val charset = getCharset()
    buf.close()
    charset
  }
  def detectString(str: String): Option[String] = {
    detector.setText(str.getBytes())
    getCharset()
  }
  private def getCharset() = {
  	val result = detector.detect()    
    if (result.getConfidence() < 50) {
      logger.error("Poor confidence of %s: %d", result.getName, result.getConfidence)
      None
    } else {
      Some(result.getName)
    }
  } 
}