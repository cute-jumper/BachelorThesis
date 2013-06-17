package thu.ailab.template

import thu.ailab.global.MyConfigFactory
import thu.ailab.sequence._
import thu.ailab.distance.LCSWithPath
import thu.ailab.tree.TreeBuilder
import thu.ailab.global.AppEntry
import thu.ailab.global.LoggerTrait
import scala.collection.mutable.ArrayBuffer

class TemplateManager private (val templates: Seq[Template]) extends LoggerTrait {
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
  def getTemplates() = templates
  override def toString() = templates.mkString("\n%s\n".format("#" * 80))
}

object TemplateManager {
  def buildTemplates() = {
    val dataset = MyConfigFactory.getValue[String]("global.dataset")
    val id2filename = io.Source.fromFile(
      MyConfigFactory.getValue[String](dataset, "output.id2filename")).getLines.toArray
    val templateFile = MyConfigFactory.getValue[String](dataset, "template.templateFile")
    def ClusterFileReader() = {
      val clusterXML = xml.XML.loadFile(
        MyConfigFactory.getValue[String](dataset, "output.clusterFile"))
      for (c <- clusterXML \ "cluster") yield (c.attributes("center").text.toInt, c \ "point" map (_.text.toInt))
    }
    val clusterFileIds = ClusterFileReader()
    def getClusterTemplate(centerId: Int, fileIds: Seq[Int]) = {
      val tnArray = new ClusterMethod(centerId, fileIds).getTemplateNodeArray
      val tp = new Template(tnArray, id2filename(centerId))
      tp
    }
    val templates = clusterFileIds.map(ids =>
      Function.tupled(getClusterTemplate _)(ids))
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
    new TemplateManager((templatesXML \ "template" map (Template.fromXML(_))))
  }
  def recoverTemplates(templateFile: String) = {
    val templatesXML = scala.xml.XML.loadFile(templateFile)
    new TemplateManager((templatesXML \ "template" map (Template.fromXML(_))))
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
  def testSaveToXML() {
    val filenames = thu.ailab.utils.Tools.getTestFiles.slice(0, 100)
    val templateArray = TemplateManager.recoverTemplates()
    val sb = new StringBuilder
    for (fn <- filenames) {
      val vtnArray = new TreeBuilder(fn).getVerboseTagSequence.toArray
      val thatTagSeq = TagSequence.fromNodeArray(vtnArray, false)
      val tpOption = templateArray.chooseTemplate(thatTagSeq)
      if (tpOption.isDefined) {
        val tp = tpOption.get
        val exPattern = tp.extract(thatTagSeq)
        sb ++= "<document name=\"%s\">\n".format(fn)
        for ((exType, content) <- exPattern) {
          sb ++= "<%s>%s</%s>\n".format(exType.toString(), content, exType.toString)
        }
        sb ++= "</document>\n"
        println("Processing " + fn)
      } else {
        println("No template found of %s!".format(fn))
      }
    }
    import thu.ailab.utils.Tools.withPrintWriter
    withPrintWriter(sys.props("user.home") + "/tmp/results.xml") { pw =>
      pw.println("<documents>")
      pw.println(sb)
      pw.println("</documents>")
    }
  }
  def testBuild() {
    println(TemplateManager.buildTemplates)
  }
  testSaveToXML()
}
