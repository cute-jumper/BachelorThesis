package thu.ailab.template

import scala.collection.mutable.ArrayBuffer

import thu.ailab.tree.TreeNode
import thu.ailab.global.MyConfigFactory
import thu.ailab.document.TagSequence

class TagSeqShingles (tagSeq: TagSequence) {
  val treeNodeArray = tagSeq.getSeparate
  val shingleLength = MyConfigFactory.getValue[Int]("template.shingleLength")
  private var preDepth = -1
  private val nodeStack = new ArrayBuffer[TreeNode]
  private val shingleBuffer = new ArrayBuffer[Shingle]
  private val fatherArray = new ArrayBuffer[TreeNode]
  private var curFather = treeNodeArray.head
  /*
  for (tn <- treeNodeArray) {
    if (tn.depth > preDepth) {
      nodeStack += tn
      fatherArray += tn
    } else {
      shingleBuffer ++= nodeStack.grouped(shingleLength).map { x =>
        new Shingle(x.toArray, curFather)
      }
      val trimLen = fatherArray.reverseIterator.takeWhile(_.depth >= tn.depth).size
      fatherArray.trimEnd(trimLen)
      curFather = fatherArray.last
      fatherArray += tn
      nodeStack.clear()
      nodeStack += tn      
    }
    preDepth = tn.depth
  }
  if (!nodeStack.isEmpty) {
    shingleBuffer ++= nodeStack.grouped(shingleLength).map { x =>
      new Shingle(x.toArray, curFather)
    }
  }
  */
  val shingles = shingleBuffer.toArray
  override def toString() = {
    shingles mkString " | "
  }
}