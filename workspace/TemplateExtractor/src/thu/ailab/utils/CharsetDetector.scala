package thu.ailab.utils

case class CharsetNotConfidentException(charset: String, confidence: Int) extends Exception

/**
 * Simple wrapper of icu4j, which can used to automatically detect
 * the character set of the HTML documents.   
 * 
 * Note: If the confidence is not good enough, say, lower than 50,
 * it will throw an exception and the caller should catch it. 
 */
object CharsetDetector {
  def apply(fn: String) = {
    import java.io.{BufferedInputStream, FileInputStream}
    import com.ibm.icu.text.{CharsetMatch, CharsetDetector}
    val detector = new CharsetDetector
    val buf = new BufferedInputStream(new FileInputStream(fn))
    detector.setText(buf)
    val result = detector.detect()
    if (result.getConfidence() < 50) {
      throw CharsetNotConfidentException(result.getName(), result.getConfidence())
    }
    buf.close()
    result.getName
  }
}