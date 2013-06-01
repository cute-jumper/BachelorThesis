package thu.ailab.document

import java.io.File

import thu.ailab.global._
import thu.ailab.sequence.TagSequence
import thu.ailab.tree.{TreeNode, TreeBuilder, HTMLSuffixTree}
import thu.ailab.utils.Tools.withPrintWriter

object DocProcessor {
  def HTMLToTagSequence() = {
    val blogdir = MyConfigFactory.getValue[String]("document.blogdir")
    val blogFilenames = new File(blogdir).listFiles().map(_.getAbsolutePath())
    val prepBlogdir = MyConfigFactory.getValue[String]("preprocess.blogdir")
    val f = new File(prepBlogdir)
    if (!f.exists()) {
      f.mkdir()
    }
    for (fn <- blogFilenames) {
      val tagSeq = new TagSequence(new TreeBuilder(fn).getTagSequence.toArray, false)
      xml.XML.save(fn.replace(blogdir, prepBlogdir), tagSeq.toXML)
    }
  }
}

object TestDocProcessor extends AppEntry {
	DocProcessor.HTMLToTagSequence
}