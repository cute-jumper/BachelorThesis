package thu.ailab.cluster

import thu.ailab.sequence.TagSeqFactory
import thu.ailab.tree._
import thu.ailab.global._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{HashSet => MHashSet, HashMap => MHashMap}

class DocNaiveAggloCluster extends NaiveAggloCluster {
	val id2filename = scala.io.Source.fromFile(
			MyConfigFactory.getValue[String]("output.id2filename")).getLines.toArray
	val tagSeqFactory = new TagSeqFactory(id2filename)
	val distArray = new Array[Double]((tagSeqFactory.getSize - 1) * 
			tagSeqFactory.getSize / 2)
  for (
    (line, idx) <- scala.io.Source.fromFile(
      MyConfigFactory.getValue[String]("output.distFile")).getLines.zipWithIndex
  ) {
    distArray(idx) = line.toDouble
  }
  override def clustersInit() = {
  	val clusters = new MHashMap[Int, Cluster]
    for (i <- 0 until tagSeqFactory.getSize)
    	clusters += i -> new Cluster(new ClusterPoint(i))
    clusters
  }
  override def getDistance(id1: Int, id2: Int) = {
    	def getIndex(id1: Int, id2: Int) = {
    		if (id1 < id2) (id2 - 1) * id2 / 2 + id1
    		else (id1 - 1) * id1 / 2 + id2
    	}
      distArray(getIndex(id1, id2))
    }
	  
  override def clustering() = {
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
  def clusterToString(c: Cluster) = {
      "Cluster Center: %s\n".format(tagSeqFactory.getFilename(c.centerId)) +
      c.cps.map(c => tagSeqFactory.getFilename(c.id)).mkString("\n")
  }
  def clusterToXML(c: Cluster, verbose: Boolean = true) = {
    <cluster center={if (verbose) tagSeqFactory.getFilename(c.centerId) 
      else c.centerId.toString}>
      {if (verbose)
        for (cp <- c.cps) yield <point>{tagSeqFactory.getFilename(cp.id)}</point>
      else for (cp <- c.cps) yield <point>{cp.id}</point>}
    </cluster>
  }
  import thu.ailab.utils.Tools.withPrintWriter  
  def writeClusterFile(filename: String) = {
    withPrintWriter(filename){ pw =>
      clusters.foreach(x => pw.println(x._1 + "\n" + x._2))
    }
  }
  def writeClusterXML(filename: String) = {
    xml.XML.save(filename, 
        <clusters>
        {for (c <- clusters) yield clusterToXML(c._2, false)}
        </clusters>)
  }
}

object TestNaiveAggloCluster extends AppEntry {
  import thu.ailab.utils.Tools.timeIt
  val naive = new DocNaiveAggloCluster
  println(timeIt(naive.clustering)._2)
  naive.writeClusterXML(MyConfigFactory.getValue[String]("output.clusterFile"))
}
