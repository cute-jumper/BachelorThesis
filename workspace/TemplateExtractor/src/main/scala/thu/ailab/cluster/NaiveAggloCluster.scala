package thu.ailab.cluster

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{HashSet => MHashSet, HashMap => MHashMap}
import thu.ailab.global._

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
  protected val clusters = new MHashMap[Int, Cluster]
  (0 until initSize).foreach { i =>
    clusters += i -> new Cluster(new ClusterPoint(i))
  }
  var minDist = Double.MaxValue
  var minPair = (0, 0)
  /**
   * Functions and classes' definitions begin
   */
  def getClusters = clusters

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
  val pairs = (for (i <- 0 until initSize; j <- 0 until i) yield ((i, j))).sortBy(x => getDistance(x._1, x._2))
  val reserve = Array.fill(initSize)(true)
  def findNearest() = {
    val bank = new MHashSet[Cluster]
    var minDist = Double.MaxValue
    var minPairId = (-1, -1)
    val ids = clusters.values.map(_.centerId).toArray
    for (i <- ids.indices; j <- 0 until i) {
      val dist = getDistance(ids(i), ids(j))
      if (dist < minDist) {
        minDist = dist
        minPairId = (ids(i), ids(j))
      }
    } 
    (minPairId, minDist)
  }  
  def clustering() = {
    import scala.annotation.tailrec
    @tailrec
    def tailrecClustering(clusters: MHashMap[Int, Cluster], times: Int) {
      val findRes = pairs.find(x => reserve(x._1) && reserve(x._2)) 
      if (findRes.isEmpty) {
        logger.info("all in one")
        return
      }
      val minPairId = findRes.get
      val minDist = getDistance(minPairId._1, minPairId._2)
      if (minDist > clusterThreshold) {
        logger.info("minDist: %f ".format(minDist) + minPairId)
        return
      }
      val newCluster = clusters(minPairId._1).mergeCluster(clusters(minPairId._2))
      clusters.remove(minPairId._1)
      clusters.remove(minPairId._2)
      clusters(newCluster.centerId) = newCluster
      reserve(minPairId._1) = false;reserve(minPairId._2) = false;
      reserve(newCluster.centerId) = true;
      tailrecClustering(clusters, times + 1)      
    }
    tailrecClustering(clusters, 0)
  }
  class ClusterPoint(val id: Int) {
    var distSum: Double = 0.0
  }
  class Cluster(cp: ClusterPoint) {
    var centerId: Int = cp.id
    protected val cps = ArrayBuffer[ClusterPoint](cp)
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
    def getPoints() = cps 
    def getPointCount() = cps.size
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
