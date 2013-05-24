package thu.ailab.template

import scala.collection.mutable.LinkedList

import thu.ailab.global.MyConfigFactory
import thu.ailab.document.TagSeqFactory
import thu.ailab.tree.TreeNode

class SeedMethod {
  def calcTreeNodeArrayLength(treeNodeArray: Array[TreeNode]) = {
    treeNodeArray.foldLeft(0){(acc, node) =>
      acc + (if (node.allowMultiple) node.innerSize * 2 else node.innerSize)
    }
  }
  val id2filename = scala.io.Source.fromFile(
      MyConfigFactory.getValue[String]("output.id2filename")).getLines.toArray
  val tagSeqFactory = new TagSeqFactory(id2filename)
  val documents = tagSeqFactory.getAllInstances.sortBy(calcTreeNodeArrayLength)
  val seed = new LinkedList(documents.head.toList)
  for (doc <- documents.tail) {
    
  }
}