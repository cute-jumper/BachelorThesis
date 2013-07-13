package thu.ailab.document

import java.io.File
import thu.ailab.global._
import thu.ailab.sequence.TagSequence
import thu.ailab.tree.{TreeNode, TreeBuilder, HTMLSuffixTree}
import thu.ailab.utils.Tools.{withPrintWriter, getTrainFiles}
import java.io.FileInputStream
import java.io.FileOutputStream

/**
 * Document preprocessing
 */
object DocProcessor extends LoggerTrait {
  val dataset = MyConfigFactory.getValue[String]("global.dataset")
  val docDir = MyConfigFactory.getValue[String](dataset, "document.dir")
  val prepDir = MyConfigFactory.getValue[String](dataset, "preprocess.dir")
  val errorPageMaxLength = MyConfigFactory.getValue[Long](dataset, "errorPageMaxLength")
  /**
   * Convert the original document to TagSequence and store the results.
   */
  def HTMLToTagSequence() = {
    val filenames = getTrainFiles
    val f = new File(prepDir)
    if (!f.exists()) {
      f.mkdir()
    }
    for (fn <- filenames) {
      val tagSeq = TagSequence.fromNodeArrayForPrep(new TreeBuilder(fn).getTagSequence.toArray, false)
      xml.XML.save(fn.replace(docDir, prepDir), tagSeq.toXML)
    }
    /**
     * Store the statistics
     */
    val statMap = HTMLSuffixTree.getStat
    logger.info("-" * 10 + "begin stat" + "-" * 10)
    for (len <- statMap.keys.toSeq.sortBy(identity); count = statMap(len)) {
      logger.info(len + " : " + count)
    }
    logger.info("-" * 10 + "end stat" + "-" * 10)
  }
  /**
   * Copy file using "Channel", faster than "IO Stream".
   * FileChannel is a class defined in java.nio.channels
   */
  private def copyFile(srcFile: String, destFile: String) = {
    val fin = new FileInputStream(srcFile).getChannel()
    val fout = new FileOutputStream(destFile).getChannel()
    fin.transferTo(0, fin.size(), fout)
    fin.close()
    fout.close()
  }
  /**
   * Get detailed pages from all the pages and store them in a directory
   */
  def filterDetailPages(copy: Boolean = false) = {
    def getDetailPage(files: Array[File]) = {
      files.filterNot(isErrorPageByLength).map(_.getAbsolutePath()).
      filterNot(x => isContentsPageByUrl(x))
    }
    val files = new File(docDir).listFiles()
    println("total size: " + files.size)
    val detailFiles = getDetailPage(files)
    println("detail size: " + detailFiles.size)
    if (copy) {
      val detailDir = docDir + "_detail"
      val f = new File(detailDir)
      if (f.exists || f.mkdir()) {
        detailFiles.foreach(x => copyFile(x, x.replace(docDir, detailDir)))
      }      
    }
  }
  /**
   * Helper functions to judge a page type 
   */
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