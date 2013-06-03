package thu.ailab.template

import thu.ailab.global.MyConfigFactory
import thu.ailab.sequence._
import thu.ailab.distance.LCSWithPath


class TemplateManager private(val templates: Seq[Template]) {
	def chooseTemplate(thatTagSeq: TagSequence): Option[Template] = {
	  for (t <- templates) {
	    if (t.isMatched(thatTagSeq))
	      return Some(t)
	  }
	  None
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
      val tp = new Template(tnArray, centerId)
      tp
    }
    val templates = clusterFileIds.map(ids => 
      Function.tupled(getClusterTemplate _)(ids)).sortBy(-_.getETagSeqLength)
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
    new TemplateManager((templatesXML \ "template" map (Template.fromXML(_))).
        sortBy(_.getETagSeqLength)(Ordering[Int].reverse))
  }
}