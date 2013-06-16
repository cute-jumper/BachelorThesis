package controllers

import org.jsoup.Jsoup
import org.jsoup.select.NodeVisitor
import org.jsoup.nodes._
import thu.ailab.document.WebHTMLDocument
import thu.ailab.tree.TreeBuilder
import thu.ailab.sequence.TagSequence
import thu.ailab.template.TemplateManager
import thu.ailab.tree.VerboseTreeNode

object TemplateExtractor {
  println("Init started")
  val templateMgr = TemplateManager.recoverTemplates(sys.props("user.home") + "/Programs/BachelorThesis/Data/material/templateFile_news_0.4")
  println("Init done")
  def feed(html: String) = {
    println("web doc")
    val htmlDoc = new WebHTMLDocument(html)
    println("start parse")
    val doc = Jsoup.parse(htmlDoc.simplifiedContent, htmlDoc.charset)
    val vnodeArray = new TreeBuilder(doc).getVerboseTagSequence.toArray
    val thatTagSeq = TagSequence.fromNodeArray(vnodeArray, false)
    println("start choosing")
    val templateOption = templateMgr.chooseTemplate(thatTagSeq)
    if (templateOption.isDefined) {
      val template = templateOption.get
      val (matchedNodes, _) = template.getMatchedNodesAndExPattern(thatTagSeq)
      val nodePool = matchedNodes.getCompact.map(
          _.asInstanceOf[VerboseTreeNode]).flatMap(_.relatedRoots).toSet
      doc.traverse(new RenderVisitor(nodePool))
      Some(doc.html)
    } else {
      None
    }
  }
  class RenderVisitor(nodePool: Set[Node]) extends NodeVisitor {
    val styleTemplate = "outline: %s dashed thin;box-shadow: 0 0 1px 2px #00f;"
    def getColor(depth: Int) = {
      val half = depth / 2
      if (half > 16) "#fff"
      else "#" + "%x".format(half) * 3
    }    
    def head(node: Node, depth: Int) = {
      if (nodePool.contains(node))
        node.attr("style", styleTemplate.format(getColor(depth)))
    }
    def tail(node: Node, depth: Int) = {
      if (nodePool.contains(node)) {
        println(node.attributes())
      }
    }
  }
}

