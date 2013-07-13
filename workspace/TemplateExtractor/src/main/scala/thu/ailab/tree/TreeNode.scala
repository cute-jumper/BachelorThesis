package thu.ailab.tree

import org.jsoup.nodes._
import scala.collection.mutable.ArrayBuffer
import thu.ailab.template.ExType._

/**
 * Representation of an array of nodes, 
 * including information like node name and node depth 
 * 
 * Two forms:
 * 1. An array of nodes, which could not be repeated.
 * 2. An array of nodes, which could be repeated
 */
class TreeNode(val nameArray: Array[String],
    val depthArray: Array[Int], 
    val allowMultiple: Boolean,
    val exType: ExType = MAGIC) {
  def this(name: String, depth: Int, allowMultiple: Boolean = false) = {
    this(Array[String](name), Array[Int](depth), allowMultiple)
  }
  def this(treeNode: TreeNode, allowMultiple: Boolean) = {
    this(treeNode.nameArray, treeNode.depthArray, allowMultiple)
  }
  private val hashCodePairs = nameArray zip depthArray map { x =>
    41 * (41 + x._1.hashCode()) + x._2.hashCode()
  }
  private val isDuplicate = !hashCodePairs.exists(_ != hashCodePairs(0))
  val name = if (isDuplicate) nameArray(0) else nameArray.mkString
  val minDepth = depthArray(0)
  val maxDepth = depthArray.max
  val depth = minDepth
  val innerSize = if (isDuplicate) 1 else nameArray.size

  def shallowEquals(other: Any) = {
    other match {
      case that: TreeNode =>
        (that canEqual this) && this.hashCode == that.hashCode
      case _ => false
    }
  }

  def getSeparateNodes() = {
    nameArray zip depthArray map { x =>
      new TreeNode(x._1, x._2)
    }
  }
  
  def merge(treeNodeArray: Array[TreeNode]) = {
    val startValue = (this.nameArray, this.depthArray)
    val paramPair = treeNodeArray.foldLeft(startValue){(acc, x) => 
      (acc._1 ++ x.nameArray, acc._2 ++ x.depthArray)
    }
    new TreeNode(paramPair._1, paramPair._2, true)
  }  
  
  /**
   * object equality methods
   */
  // Starts here
  override def equals(other: Any) = {
    other match {
      case that: TreeNode => 
        (that canEqual this) && this.hashCode == that.hashCode
      case _ => false
    }
  }
  override val hashCode = {
    if (isDuplicate)
      hashCodePairs(0)
    else
      hashCodePairs.foldLeft(1)((acc, x) => 41 * acc + x)
  }
  def canEqual(that: Any) = that.isInstanceOf[TreeNode]
  // Ends here
  
  override val toString = {
    if (isDuplicate)
      name + "." + depth
    else
      nameArray.zip(depthArray).map(x => x._1 + "." + x._2).mkString("-")
  }
  
  def toXML() = {
    <treenode allowMultiple={allowMultiple.toString} exType={exType.toString}>
      <names>
      {for (name <- nameArray) yield <name>{name}</name>}
      </names>
      <depths>
      {for (depth <- depthArray) yield <depth>{depth}</depth>}
      </depths>
    </treenode>
  }
}

object TreeNode {
  def fromXML(node: scala.xml.Node) = {
    val nameArray = (node \ "names" \ "name" map (_.text)).toArray
    val depthArray = (node \ "depths" \ "depth" map (_.text.toInt)).toArray
    val allowMultiple = node.attribute("allowMultiple").get.text.toBoolean
    val exTypeOption = node.attribute("exType")
    val exType = 
      if (exTypeOption.isDefined) withName(exTypeOption.get.text)
      else MAGIC
    new TreeNode(nameArray, depthArray, allowMultiple, exType)
  }
}

/**
 * Add the corresponding original Jsoup nodes to the TreeNode to
 * form a more verbose version of TreeNode class. Others are similar.
 * 
 * ATTENTION when we deal with a series of VerboseTreeNodes:
 * We would first convert `VerboseTreeNode' to `TreeNode' when we want
 * to use various functions defined only for `TreeNode', especially when
 * the input of a function is like "Array of TreeNode". When we finally
 * need a `VerboseTreeNode', we convert it back from `TreeNode'. This
 * convention requires ourselves to remember which nodes are actually
 * `VerboseTreeNode' and which are not. It is not convenient. However, 
 * this problem *SEEMS* inevitable due to the complexity to deal with
 * problems like "covariance" or "contravariance" and the restriction of
 * JVM's generics.
 * 
 * The Jsoup nodes will be stored in the variable `relatedRoots'. 
 */
class VerboseTreeNode(override val nameArray: Array[String], 
    override val depthArray: Array[Int], 
    override val allowMultiple: Boolean,
    val relatedRoots: ArrayBuffer[Node]) extends TreeNode(nameArray, depthArray, allowMultiple) {
  def this(name: String, depth: Int, relatedRoot: Node) = {
    this(Array(name), Array(depth), false, ArrayBuffer(relatedRoot))
  }
  def merge(treeNodeArray: Array[VerboseTreeNode]) = {
    val startValue = (this.nameArray, this.depthArray)
    val paramPair = treeNodeArray.foldLeft(startValue){(acc, x) => 
      (acc._1 ++ x.nameArray, acc._2 ++ x.depthArray)
    }
    new VerboseTreeNode(paramPair._1, paramPair._2, true, this.relatedRoots)
  }
  def addRelatedRoot(that: VerboseTreeNode) = {
    relatedRoots ++= that.relatedRoots
  }
  def removeLastRelatedRoot() = {
    relatedRoots.trimEnd(1)
  }
}

object TestTreeNode extends App {
  import scala.collection.immutable.HashMap
  val m = HashMap[TreeNode, Int](new TreeNode("div", 3) -> 2)
  m.keys.foreach(x => println(x.hashCode))
  println(new TreeNode("div", 3).hashCode)
  println(m.contains(new TreeNode("div", 3)))
}