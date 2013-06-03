package thu.ailab.template

import thu.ailab.global.MyConfigFactory
import thu.ailab.sequence.TagSeqFactory


class TemplateManager private(val templates: Seq[Template]) {
	def chooseTemplate(thatTagSeq) = {
	  
	}
}

object TemplateManager {
  def buildTemplates() = {
    val id2filename = io.Source.fromFile(
      MyConfigFactory.getValue[String]("output.id2filename")).getLines.toArray
    val tagSeqFactory = new TagSeqFactory(id2filename)
    val templateFile = MyConfigFactory.getValue[String]("template.templateFile")
    def ClusterFileReader() = {
      val clusterXML = xml.XML.loadFile(
        MyConfigFactory.getValue[String]("output.clusterFile"))
      for (c <- clusterXML \ "cluster") yield (c.attributes("center").text.toInt, c \ "point" map (_.text.toInt))
    }
    val clusterFileIds = ClusterFileReader()
    def getClusterTemplate(centerId: Int, fileIds: Seq[Int]) = {
      val tnArray = new ClusterMethod(centerId, fileIds).getTemplateNodeArray
      val tp = new Template(tnArray)
      tp
    }
    val templates = clusterFileIds.map(ids => Function.tupled(getClusterTemplate _)(ids))
    scala.xml.XML.save(templateFile,
      <templates>
        {
          for (tp <- templates) yield (tp.toXML)
        }
      </templates>)
    new TemplateManager(templates)
  }
  def recoverTemplates() = {
    val templateFile = MyConfigFactory.getValue[String]("template.templateFile")
    val templatesXML = scala.xml.XML.loadFile(templateFile)
    new TemplateManager(templatesXML \ "template" map (Template.fromXML(_)))
  }
}