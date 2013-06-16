package thu.ailab.template

import thu.ailab.global.MyConfigFactory
import thu.ailab.sequence._
import thu.ailab.distance.LCSWithPath
import thu.ailab.tree.TreeBuilder
import thu.ailab.global.AppEntry
import thu.ailab.global.LoggerTrait

class TemplateManager private(val templates: Seq[Template]) extends LoggerTrait {
  private val clusterThreshold = MyConfigFactory.getValue[Double](
      "cluster.DocNaiveAggloCluster.clusterThreshold")
	def chooseTemplate(thatTagSeq: TagSequence): Option[Template] = {
	  val (minDist, index) = 
	    templates.map(_.distFromCenter(thatTagSeq)).zipWithIndex.minBy(_._1)
	  if (minDist < clusterThreshold) {
	    logger.info("choose template " + index)
	    Some(templates(index))
	  } else 
	    None
	}
	override def toString() = templates.mkString("\n%s\n".format("#" * 80))
}

object TemplateManager {
  def buildTemplates() = {
    val dataset = MyConfigFactory.getValue[String]("global.dataset")
    val id2filename = io.Source.fromFile(
      MyConfigFactory.getValue[String](dataset, "output.id2filename")).getLines.toArray
    val tagSeqFactory = new TagSeqFactory(id2filename)
    val templateFile = MyConfigFactory.getValue[String](dataset, "template.templateFile")
    def ClusterFileReader() = {
      val clusterXML = xml.XML.loadFile(
        MyConfigFactory.getValue[String](dataset, "output.clusterFile"))
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
    val dataset = MyConfigFactory.getValue[String]("global.dataset")
    val templateFile = MyConfigFactory.getValue[String](dataset, "template.templateFile")
    val templatesXML = scala.xml.XML.loadFile(templateFile)
    new TemplateManager((templatesXML \ "template" map (Template.fromXML(_))).
        sortBy(_.getETagSeqLength)(Ordering[Int].reverse))
  }
}

object TestTemplateManager extends AppEntry {
  def testExtract() {
    val fn = thu.ailab.utils.Tools.getRandomTestFile
    val vtnArray = new TreeBuilder(fn).getVerboseTagSequence.toArray
    val thatTagSeq = TagSequence.fromNodeArray(vtnArray, false)
    val templateArray = TemplateManager.recoverTemplates()
    val tpOption = templateArray.chooseTemplate(thatTagSeq)
    if (tpOption.isDefined) {
      val tp = tpOption.get
      tp.extract(thatTagSeq)
    } else {
      println("No template found!")
    }
  }
  def testBuild() {
    println(TemplateManager.buildTemplates)
  }
  testExtract()
}
