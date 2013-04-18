package thu.ailab.preprocess

import thu.ailab.utils.CharsetDetector
import thu.ailab.config.MyConfigFactory
import thu.ailab.global.LoggerTrait

/**
 * Document class, representing one HTML document, with some useful information. 
 * It is *NOT* DOM or SAX, though.
 * 
 * Important fields:
 *  - filename
 *  - url
 *  - charset
 *  - fullContent(allLines)
 *  - simplifiedContent(strippedLines)
 */

class RawDocument(val filename: String) extends LoggerTrait {
  /**
   *  Since we mainly deal with Chinese documents, default is gb18030
   */ 
  import thu.ailab.utils.Tools.getFileCharset
  val charset = getFileCharset(filename).getOrElse("gb18030")
  val url = java.net.URLDecoder.decode(filename, charset)
  logger.debug("%s Charset: %s", filename, charset)
  
  /**
   * Get full content, with blank lines stripped
   */
  val allLines = io.Source.fromFile(filename)(scala.io.Codec(charset)).getLines
  val fullContent = allLines map (_.trim) filter (_.length != 0) mkString "\n"
  
  /**
   * Remove tags according to the regular expression generator
   */
  private def removeTags(tags: List[String], content: String)(regexGen: (String) => String) = {
    def removeTagsImpl(tags: List[String], content: String): String = {
      def removeTag(tag: String) = content.replaceAll(regexGen(tag), "")
      tags match {
        case Nil => content
        case tag::rest => removeTagsImpl(rest, removeTag(tag))
      }
    }
    removeTagsImpl(tags, content)
  }
  /**
   * Remove paired tags, like <script>...</script>
   */
  private def removePairedTags(tags: List[String])(content: String) = {
	removeTags(tags, content)((tag: String) => """(?is)<%s.*?>.*?</%s>""".format(tag, tag))
  }
  /**
   * Remove single tags, like <br/>, <input ../>
   */
  private def removeSingleTags(tags: List[String])(content: String) = {
    removeTags(tags, content)("""(?is)<%s.*?/[ ]*>""".format(_))
  }
  
  /**
   * Remove *ONLY* tags, not content
   */
  private def removeOnlyTags(tags: List[String])(content: String) = {
    removeTags(tags, content)("""(?is)<[/]?%s.*?>""".format(_))
  }
  /**
   * Simply remove all the patterns in the list
   */
  private def removePatterns(patterns: List[String])(content: String): String = {
    patterns match {
      case Nil => content
      case pattern::rest => removePatterns(rest)(content.replaceAll(pattern, ""))
    }
  } 
  
  private val pairedTags = List("script", "style", "p")
  private val singleTags = List("link", "input", "br", "img")
  private val uselessTags = List("strong", "em", "font", "b")
  private val uselessPatterns = List("""(?is)<!--.*?-->""")
  /**
   * Compose all the removal functions into one
   */
  private val removeAll = List(removeSingleTags(singleTags)_,
		  removePairedTags(pairedTags)_,
		  removeOnlyTags(uselessTags)_,
		  removePatterns(uselessPatterns)_) reduce (_ compose _)
  val strippedLines = removeAll(fullContent) split "\n" map (_.trim) filter (_.length != 0)
  val simplifiedContent =  strippedLines mkString "\n"
  
  def test() = {
    println(simplifiedContent)
  }
}

object RawDocument {
  private val fileLoaded = Array[Boolean]()
  def main(args: Array[String]) {
    MyConfigFactory
    val foo = new RawDocument("../../Data/blog1000/http%3A%2F%2Fblog.sina.com.cn%2Fs%2Fblog_00f2e45101017icv.html")
    //foo.test()
  }
}
