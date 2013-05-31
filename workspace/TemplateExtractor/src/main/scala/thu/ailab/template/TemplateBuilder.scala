package thu.ailab.template

import scala.collection.mutable.{HashMap => MHashMap}

import thu.ailab.global.MyConfigFactory
import thu.ailab.tree._
import thu.ailab.sequence.TagSeqFactory

class TemplateBuilder {
  val id2filename = io.Source.fromFile(MyConfigFactory.getValue[String](
      "output.id2filename")).getLines.toArray
  val tagSeqFactory = new TagSeqFactory(id2filename)
  def ClusterFileReader() = {
    val clusterXML = xml.XML.loadFile(
        MyConfigFactory.getValue[String]("output.clusterFile"))
    for (c <- clusterXML \ "cluster") yield 
    (c.attributes("center").text.toInt, c \ "point" map (_.text.toInt))
  }
  val clusterFileIds = ClusterFileReader()
  def getClusterTemplate(centerId: Int, fileIds: Seq[Int]) = {
    /*
    val shingleMap = new MHashMap[Shingle, Int]
    val tssArray = fileIds.map { id =>
      new TagSeqShingles(tagSeqFactory.getInstance(id))
    }
    for (tss <- tssArray) {
      for (shingle <- tss.shingles)
        shingleMap(shingle) = shingleMap.getOrElse(shingle, 0) + 1
    }
    val chooseId = 3
    println(tagSeqFactory.getFilename(chooseId))
    println(new TreeBuilder(tagSeqFactory.getFilename(chooseId)).getTagSequence.mkString(" "))
    for (shingle <- tssArray(chooseId).shingles) {
      println("%4d : %s".format(shingleMap(shingle), shingle))
    }
    */
    new CenterMethod(centerId, fileIds)
  }
  Function.tupled(getClusterTemplate _)(clusterFileIds(2))
}

object TestTemplateBuilder extends App {
  new TemplateBuilder
}