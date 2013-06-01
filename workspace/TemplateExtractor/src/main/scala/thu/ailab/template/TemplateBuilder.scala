package thu.ailab.template

import scala.collection.mutable.{HashMap => MHashMap}
import thu.ailab.global._
import thu.ailab.tree._
import thu.ailab.sequence.TagSeqFactory

class TemplateBuilder {    
  val id2filename = io.Source.fromFile(
      MyConfigFactory.getValue[String]("output.id2filename")).getLines.toArray
  val tagSeqFactory = new TagSeqFactory(id2filename)
  val templateFile = MyConfigFactory.getValue[String]("template.templateFile")
  def ClusterFileReader() = {
    val clusterXML = xml.XML.loadFile(
        MyConfigFactory.getValue[String]("output.clusterFile"))
    for (c <- clusterXML \ "cluster") yield 
    (c.attributes("center").text.toInt, c \ "point" map (_.text.toInt))
  }
  val clusterFileIds = ClusterFileReader()
  def getClusterTemplate(centerId: Int, fileIds: Seq[Int]) = {
    val tnArray = new ClusterMethod(centerId, fileIds).getTemplateNodeArray
    val tp = new Template(tnArray)
    tp
  }
  scala.xml.XML.save(templateFile,
      <templates>{for (ids <- clusterFileIds) yield 
    (Function.tupled(getClusterTemplate _)(ids).toXML)}
      </templates>)
}

object TestTemplateBuilder extends AppEntry {
  new TemplateBuilder
}