package thu.ailab.document

import java.io.File

import thu.ailab.global._
import thu.ailab.sequence.TagSequence
import thu.ailab.tree.{TreeNode, TreeBuilder, HTMLSuffixTree}
import thu.ailab.utils.Tools.withPrintWriter

object DocProcessor {
  private val dataset = MyConfigFactory.getValue[String]("global.dataset")
  private val docDir = MyConfigFactory.getValue[String](dataset, "document.dir")
  private val prepDir = MyConfigFactory.getValue[String](dataset, "preprocess.dir")
  private val errorPageMaxLength = MyConfigFactory.getValue[Long](dataset, "errorPageMaxLength")
  def HTMLToTagSequence() = {
    val filenames = new File(docDir).listFiles().map(_.getAbsolutePath())
    val f = new File(prepDir)
    if (!f.exists()) {
      f.mkdir()
    }
    for (fn <- filenames) {
      val tagSeq = TagSequence.fromNodeArray(new TreeBuilder(fn).getTagSequence.toArray, false)
      xml.XML.save(fn.replace(docDir, prepDir), tagSeq.toXML)
    }
  }
  def getDetailPage(dir: String) = {
    new File(dir).listFiles().filterNot(isErrorPageByLength).map(_.getAbsolutePath()).
    filterNot(x => isContentsPageByUrl(x))/* || 
        isErrorPageByContent(scala.io.Source.fromFile(x).getLines.mkString("")))
        */
  }  
  def isContentsPageByUrl(url: String) = {
    val dataset = MyConfigFactory.getValue[String]("global.dataset")
    val pattern = MyConfigFactory.getValue[String](dataset, "pattern.contentsPageUrlPattern")
    (pattern.r findFirstIn url).isDefined
  }
  def isErrorPageByContent(content: String) = {
    content.toLowerCase.contains("404 not found")
  }
  def isErrorPageByLength(file: File) = {
    file.length <= errorPageMaxLength
  }
}

object TestDocProcessor extends AppEntry {
	DocProcessor.HTMLToTagSequence
}