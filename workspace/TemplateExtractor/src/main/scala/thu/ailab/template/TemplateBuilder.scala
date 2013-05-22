package thu.ailab.template

import scala.collection.mutable.{HashMap => MHashMap}

import thu.ailab.global.MyConfigFactory
import thu.ailab.tree.TreeNode
import thu.ailab.document.TagSeqFactory

class TemplateBuilder {
  val id2filename = io.Source.fromFile(MyConfigFactory.getValue[String](
      "output.id2filename")).getLines.toArray
  val tagSeqFactory = new TagSeqFactory(id2filename)
  def ClusterFileReader() = {
    val clusterXML = xml.XML.loadFile(
        MyConfigFactory.getValue[String]("output.clusterFile"))
    for (c <- clusterXML \ "cluster") yield c \ "point" map (_.text.toInt)
  }
  val clusterFileIds = ClusterFileReader()
  def getClusterTemplate(fileIds: Seq[Int]) = {
    val shingleMap = new MHashMap[Shingle, Int]
    val tssArray = fileIds.map { id =>
      new TagSeqShingles(tagSeqFactory.getInstance(id))
    }
    for (tss <- tssArray) {
      for (shingle <- tss.shingles)
        shingleMap(shingle) = shingleMap.getOrElse(shingle, 0) + 1
    }
    for (shingle <- tssArray(util.Random.nextInt(tssArray.length)).shingles) {
      println("%4d : %s".format(shingleMap(shingle), shingle))
    }
  }
  getClusterTemplate(clusterFileIds(2))
}

object TestTemplateBuilder extends App {
  new TemplateBuilder
}