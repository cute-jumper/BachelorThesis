package thu.ailab.cluster

import thu.ailab.config._
import thu.ailab.tree._
import thu.ailab.global._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{HashSet => MHashSet, HashMap => MHashMap}

/**
 * I avoid using functional style here
 * because I try to minimize all the
 * overhead to make it run faster.
 */
class NaiveAggloCluster extends LoggerTrait {
  val id2filename = scala.io.Source.fromFile(MyConfigFactory.getValue[String]("output.id2filename")).getLines.toArray
  val factory = new TagSeqFactory(id2filename)
  val distArray = new Array[Double]((factory.size - 1) * factory.size / 2)
  for ((line, idx) <- scala.io.Source.fromFile(
      MyConfigFactory.getValue[String]("output.distFile")).getLines.zipWithIndex) {
    distArray(idx) = line.toDouble
  }
  val clusterThreshold = MyConfigFactory.getValue[Double]("cluster.NaiveAggloCluster.clusterThreshold")
  val clusters = new MHashMap[Int, Cluster]
  var minDist = Double.MaxValue
  var minPair = (0, 0)
  for (i <- 0 until factory.size) {
    clusters += i -> new Cluster(new ClusterPoint(i))
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
    def distFrom(that: Cluster) = 
      distArray(getIndex(this.centerId, that.centerId))
    def size = cps.size
    override def toString = {
      "Cluster Center: %s\n".format(factory.getFilename(centerId)) +
      cps.map(c => factory.getFilename(c.id)).mkString("\n")
    }
  }
  import thu.ailab.utils.Tools.withPrintWriter  
  def writeFile(filename: String) = {
    withPrintWriter(filename){ pw =>
      clusters.foreach(pw.println)
    }
  }
}

object TestNaiveAggloCluster extends AppEntry {
  import thu.ailab.utils.Tools.timeIt
  val naive = new NaiveAggloCluster
  println(timeIt(naive.clustering)._2)
  naive.writeFile(MyConfigFactory.getValue[String]("output.clusterFile"))
}

