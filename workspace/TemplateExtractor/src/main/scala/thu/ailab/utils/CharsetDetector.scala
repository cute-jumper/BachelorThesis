package thu.ailab.utils

import com.twitter.logging.Logger

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
private[utils] object CharsetDetector {
  private val logger = Logger.get(getClass)
  def apply(filename: String): Option[String] = {
    import java.io.{BufferedInputStream, FileInputStream}
    import com.ibm.icu.text.{CharsetMatch, CharsetDetector}
    val detector = new CharsetDetector
    val buf = new BufferedInputStream(new FileInputStream(filename))
    detector.setText(buf)
    val result = detector.detect()
    buf.close()
    if (result.getConfidence() < 50) {
      logger.error("Poor confidence of %s: %d", result.getName, result.getConfidence)
      None
    } else {
      Some(result.getName)
    }
  }
}