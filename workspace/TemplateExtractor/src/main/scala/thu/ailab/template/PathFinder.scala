package thu.ailab.template

import thu.ailab.sequence._
import thu.ailab.tree.TreeNode

//case class PathTuple(val before: Option[Shingle],
//    val current: Option[Shingle],
//    val after: Option[Shingle]) {
//  def this(shingle: Shingle) = {
//    this(shingle.before, Some(shingle), shingle.after)
//  }
//  def isBefore(that: PathTuple) = {
//    current == that.before && after == that.current
//  }
//  def isAfter(that: PathTuple) = that.isBefore(this)
//}
//    
//class PathFinder(val shingleSet: Set[Shingle]) {
//  val firstShingle = shingleSet.find(_.before == None).get
//  val lastShingle = shingleSet.find(_.after == None).get
//  val pathTupleSet = 
//    for (shingle <- (shingleSet - firstShingle - lastShingle)) 
//      yield new PathTuple(shingle)
//  val firstTuple = new PathTuple(firstShingle)
//  val lastTuple = new PathTuple(lastShingle)
//  val allPaths = {
//    def findAllPaths(pathTupleSet: Set[PathTuple], curPath: Array[PathTuple]): Set[Array[PathTuple]] = {
//      if (pathTupleSet.isEmpty) {
//        Set(curPath)
//      } else {
//        val prevTuple = curPath.last
//        val nextTupleSet = pathTupleSet filter { x => x.isAfter(prevTuple) }
//        (for (next <- nextTupleSet) yield {
//          findAllPaths(pathTupleSet - next, curPath :+ next)
//        }).flatten
//      }
//    }
//    findAllPaths(pathTupleSet, Array(firstTuple))
//  }  
//  val longestPath = allPaths.maxBy(_.length)
//  println(longestPath.mkString(" --> "))
//}