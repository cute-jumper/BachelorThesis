package thu.ailab.template

import thu.ailab.tree.TreeNode

case class PathTuple(val before: Option[TreeNode],
    val current: Option[TreeNode],
    val after: Option[TreeNode])
    
class PathFinder(val head: PathTuple, 
    pathTupleSet: Set[PathTuple],
    val tail: PathTuple) {
  def findAllPath(pathTupleSet: Set[PathTuple], curPath: Array[PathTuple]) {
    val prevTuple = curPath.last
    val nextTuples = pathTupleSet.collect { x => 
      x.before == prevTuple.current && x.current == prevTuple.after 
    }
  }
}