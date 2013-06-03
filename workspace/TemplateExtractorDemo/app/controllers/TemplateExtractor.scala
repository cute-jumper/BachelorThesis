package controllers

import org.jsoup.Jsoup
import thu.ailab.document.WebHTMLDocument
import thu.ailab.tree.TreeBuilder
import thu.ailab.sequence.TagSequence
import thu.ailab.template.Template

class TemplateExtractor {
  val templates = Template.recoverTemplates()
  def feed(html: String) = {
    val htmlDoc = new WebHTMLDocument(html)
    val doc = Jsoup.parse(htmlDoc.simplifiedContent, htmlDoc.charset)
    val vnodeArray = new TreeBuilder(doc).getVerboseTagSequence.toArray
    val thatTagSeq = TagSequence.fromNodeArray(vnodeArray, false)
    
  }
}

