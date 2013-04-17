package thu.ailab.cluster

import thu.ailab.config._
import thu.ailab.tree._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{HashSet => MHashSet, HashMap => MHashMap}

/**
 * I avoid using functional style here
 * because I try to minimize all the
 * overhead to make it run faster.
 */
class NaiveAggloCluster(filename: String) {
  val fdirConfig = MyConfigFactory.getConfig("MyFileDirectoriesConfig")
  val factory = new TagSeqFactory(fdirConfig.getValue("blogdir"))
  val distArray = new Array[Double](factory.size)
  for ((i, idx) <- scala.io.Source.fromFile(filename).getLines.zipWithIndex) {
    val pieces = i.split("\t")
    distArray(idx) = pieces(pieces.length - 1).toDouble
  }
  val clusterThreshold = MyConfigFactory.getConfig("MyClusterConfig").getValue("clusterThreshold")
  val clusters = new MHashSet[Cluster]
  val clusterDists = new MHashMap[Tuple2[Int, Int], Double]
  var minDist = Double.MaxValue
  var minPair = (0, 0)
  for (i <- 0 until factory.size) {
    val c = new Cluster(new ClusterPoint(i))
    for (j <- clusters) {
      val dist = distArray(getIndex(c.centerId, j.centerId))
      clusterDists += (c.centerId, j.centerId) -> dist 
      if (dist < minDist) {
        minDist = dist
        minPair = (c.centerId, j.centerId)
      }
    }
    clusters += c
  }
  def clustering() = {
    var cont = true
    while (cont) {
       
    }
  }
  def getIndex(id1: Int, id2: Int) = {
    if (id1 < id2) (id2 - 1) * id2 / 2 + id1
    else (id1 - 1) * id1 / 2 + id2
  }
  class ClusterPoint(val id: Int) {
    var distSum: Double = 0.0
  }
  class Cluster(cp: ClusterPoint) {
    var centerId: Int = cp.id
    val cps = ArrayBuffer[ClusterPoint](cp)
    def mergeCluster(that: Cluster) = {
      for (i <- this.cps; j <- that.cps) {
        val dist = distArray(getIndex(i.id, j.id))
        i.distSum += dist
        j.distSum += dist
      }
      this.cps.append(that.cps: _*)
      updateCenter
    }
    def updateCenter = {
      var minSum = Double.MaxValue
      for (i <- this.cps) {
        if (i.distSum < minSum) {
          minSum = i.distSum
          centerId = i.id
        }
      }
    }
    def clusterDistance(that: Cluster) = 
      distArray(getIndex(this.centerId, that.centerId))
  }
}



