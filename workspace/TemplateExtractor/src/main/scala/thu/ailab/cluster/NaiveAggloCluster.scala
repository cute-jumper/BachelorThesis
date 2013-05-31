package thu.ailab.cluster

import thu.ailab.global._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{HashSet => MHashSet, HashMap => MHashMap}

/**
 * I avoid using functional style here
 * because I try to minimize all the
 * overhead to make it run faster.
 */
abstract class NaiveAggloCluster extends LoggerTrait {
  val clusterThreshold = MyConfigFactory.getValue[Double](
      "cluster.NaiveAggloCluster.clusterThreshold")
  val clusters = clustersInit()
  var minDist = Double.MaxValue
  var minPair = (0, 0)
  def clustersInit(): MHashMap[Int, Cluster]  
  def clustering()
  def getDistance(id1: Int, id2: Int): Double
  /**
   * May have bugs
   */
  def findNearest() = {
    val bank = new MHashSet[Cluster]
    var minDist = Double.MaxValue
    var minPairId = (-1, -1)
    for (c <- clusters.values) {
      for (deposit <- bank) {
        val dist = c.distFrom(deposit)
        if (dist < minDist) {
          minDist = dist
          minPairId = (c.centerId, deposit.centerId)
        }
      }
      bank.add(c)
    }
    (minPairId, minDist)
  }
  class ClusterPoint(val id: Int) {
    var distSum: Double = 0.0
  }
  class Cluster(cp: ClusterPoint) {
    var centerId: Int = cp.id
    val cps = ArrayBuffer[ClusterPoint](cp)
    def mergeCluster(that: Cluster) = {
      for (i <- this.cps; j <- that.cps) {
        val dist = getDistance(i.id, j.id) 
        i.distSum += dist
        j.distSum += dist
      }
      this.cps.append(that.cps: _*)
      updateCenter
      this
    }
    def updateCenter = {
      var minSum = Double.MaxValue
      for (i <- this.cps) {
        if (i.distSum < minSum) {
          minSum = i.distSum
          centerId = i.id
        }
      }
      centerId
    }
    def distFrom(that: Cluster) = getDistance(this.centerId, that.centerId)
    def size = cps.size
  }
}
