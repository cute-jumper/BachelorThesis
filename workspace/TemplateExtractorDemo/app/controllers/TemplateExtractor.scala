package controllers

import org.jsoup.Jsoup
import org.jsoup.select.NodeVisitor
import org.jsoup.nodes._
import thu.ailab.tree.TreeBuilder
import thu.ailab.sequence.TagSequence
import thu.ailab.template.TemplateManager
import thu.ailab.tree.VerboseTreeNode
import thu.ailab.document.WebHTMLDocument
import thu.ailab.template.ExType

object TemplateExtractor {
  val templateMgr = TemplateManager.recoverTemplates(sys.props("user.home") + "/Programs/BachelorThesis/Data/material/template_blog_2000_anno.template")
  println("init done")
  def feed(html: String) = {
    val htmlDoc = new WebHTMLDocument(html)
    val doc = Jsoup.parse(htmlDoc.simplifiedContent, htmlDoc.charset)
//    val doc = Jsoup.parse(html, htmlDoc.charset)
    val vnodeArray = new TreeBuilder(doc).getVerboseTagSequence.toArray
    val thatTagSeq = TagSequence.fromNodeArray(vnodeArray, false)
    println("start choosing")
    val templateOption = templateMgr.chooseTemplate(thatTagSeq)
    if (templateOption.isDefined) {
      val template = templateOption.get
      val (matchedNodes, exPattern) = template.getMatchedNodesAndExPattern(thatTagSeq)
//      val nodePool = matchedNodes.getCompact.map(
//          _.asInstanceOf[VerboseTreeNode]).flatMap(_.relatedRoots).toSet
      doc.traverse(new RenderExPatternVisitor(exPattern))
      Some(doc.html)
    } else {
      None
    }
  }
  class RenderTemplateVisitor(nodePool: Set[Node]) extends NodeVisitor {
    val styleTemplate = "outline: %s dashed thick;box-shadow: 0 0 1px 2px #00f;"
    def getColor(depth: Int) = {
      val half = depth / 2
      if (half > 16) "#fff"
      else "#" + "%x".format(half) * 3
    }    
    def head(node: Node, depth: Int) = {
      if (nodePool.exists(x => x.baseUri == node.baseUri() 
          && x.attributes == node.attributes)) {
        node.attr("style", styleTemplate.format(getColor(depth)))
        println(node.nodeName)
      }
    }
    def tail(node: Node, depth: Int) = {
    }
  }
  import thu.ailab.template.ExType._
  class RenderExPatternVisitor(nodeMap: Map[Node, ExType]) extends NodeVisitor {
    val styleTemplate = "outline: %s dashed thick;box-shadow: 0 0 1px 2px #00f;"
    def getColor(depth: Int) = {
      val half = depth / 2
      if (half > 16) "#fff"
      else "#" + "%x".format(half) * 3
    }    
    def head(node: Node, depth: Int) = {
      val keyOption = nodeMap.keySet.find(x => x.baseUri == node.baseUri() 
          && x.attributes == node.attributes)
      if (keyOption.isDefined) {
        node.attr("style", styleTemplate.format(getColor(depth)))
        node.attr("title", nodeMap(keyOption.get).toString)
        println(node.nodeName)
      }
    }
    def tail(node: Node, depth: Int) = {
    }
  }

}

