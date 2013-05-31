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
  /**
   * Undefined fields and methods.
   * 
   * Attention: initSize is a pre-initialized field, so
   * must be initialized before super's initialization 
   */
  val initSize: Int
  val clusterThreshold: Double
  def initDistArray(): Array[Double]
  protected def composeShow(verbose: Boolean): (Int) => String
  /**
   * variables
   */
  val distArray = initDistArray()
  val clusters = new MHashMap[Int, Cluster]
  (0 until initSize).foreach { i =>
    clusters += i -> new Cluster(new ClusterPoint(i))
  }  
  var minDist = Double.MaxValue
  var minPair = (0, 0)
  /**
   * Functions and classes' definitions begin
   */
  def getIndex(id1: Int, id2: Int) = {
    if (id1 < id2) (id2 - 1) * id2 / 2 + id1
    else (id1 - 1) * id1 / 2 + id2
  }
  def getDistance(id1: Int, id2: Int) = {
    distArray(getIndex(id1, id2))
  }
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
  def clustering() = {
    import scala.annotation.tailrec
    @tailrec
    def tailrecClustering(clusters: MHashMap[Int, Cluster]) {
       val (minPairId, minDist) = findNearest
       if (minDist > clusterThreshold) {
         logger.info("minDist: %f ".format(minDist) + minPairId)
         return
       }
       val newCluster = clusters(minPairId._1).mergeCluster(clusters(minPairId._2))
       clusters.remove(minPairId._1)
       clusters.remove(minPairId._2)
       clusters(newCluster.centerId) = newCluster
       tailrecClustering(clusters)
    }
    tailrecClustering(clusters)
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
    def toStr(verbose: Boolean = true) = {
      val show = composeShow(verbose)
      "Cluster Center: %s\n".format(show(centerId)) + 
      cps.map(c => show(c.id)).mkString("\n")
    }
    def toXML(verbose: Boolean = true) = {
      val show = composeShow(verbose)
      <cluster center={ show(centerId) }>
        { for (cp <- cps) yield <point>{ show(cp.id) }</point> }
      </cluster>
    }
  }
}
