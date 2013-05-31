package thu.ailab.template

import thu.ailab.tree.TreeNode

abstract class TemplateNode

case class EssentialNode(val tnArray: Array[TreeNode]) extends TemplateNode

case class OptionalNode(tnArrays: Array[Array[TreeNode]]) extends TemplateNode {
  
}