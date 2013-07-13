package thu.ailab.template

import thu.ailab.global.MyConfigFactory
import thu.ailab.sequence._
import thu.ailab.distance.LCSWithPath
import thu.ailab.tree.TreeBuilder
import thu.ailab.global.AppEntry
import thu.ailab.global.LoggerTrait
import scala.collection.mutable.ArrayBuffer

/**
 * Manage the templates
 */
class TemplateManager private (val templates: Seq[Template]) extends LoggerTrait {
  private val clusterThreshold = MyConfigFactory.getValue[Double](
    "cluster.DocNaiveAggloCluster.clusterThreshold")
  /**
   * Choose the best matching template by the distance to the cluster center.
   */
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
  /**
   * Build templates and store them in XMLs.
   */
  def buildTemplates() = {
    val dataset = MyConfigFactory.getValue[String]("global.dataset")
    val id2filename = io.Source.fromFile(
      MyConfigFactory.getValue[String](dataset, "output.id2filename")).getLines.toArray
    val templateFile = MyConfigFactory.getValue[String](dataset, "template.templateFile")
    /**
     * Initialize clusters
     */
    val clusterFileIds = {
      val clusterXML = xml.XML.loadFile(
        MyConfigFactory.getValue[String](dataset, "output.clusterFile"))
      for (c <- clusterXML \ "cluster") yield (c.attributes("center").text.toInt, c \ "point" map (_.text.toInt))
    }
    def getClusterTemplate(centerId: Int, fileIds: Seq[Int]) = {
      val tnArray = new ClusterMethod(centerId, fileIds).getTemplateNodeArray
      val tp = new Template(tnArray, id2filename(centerId))
      tp
    }
    /**
     * For each cluster, build a template
     */
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
  /**
   * Recover templates from XMLs *AUTOMATICALLY*.
   */
  def recoverTemplates() = {
    val dataset = MyConfigFactory.getValue[String]("global.dataset")
    val templateFile = MyConfigFactory.getValue[String](dataset, "template.templateFile")
    val templatesXML = scala.xml.XML.loadFile(templateFile)
    new TemplateManager((templatesXML \ "template" map (Template.fromXML(_))))
  }
  /**
   * Recover templates from XMLs *MANUALLY*. 
   */
  def recoverTemplates(templateFile: String) = {
    val templatesXML = scala.xml.XML.loadFile(templateFile)
    new TemplateManager((templatesXML \ "template" map (Template.fromXML(_))))
  }
}

/**
 * Run various tests
 */
object TestTemplateManager extends AppEntry {
  /**
   * Randomly pick a file, and try to extract things from the file
   */
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
  /**
   * Extract things from a series of files
   */
  def testSaveToXML() {
    val dataset = MyConfigFactory.getValue[String]("global.dataset")
    val filenames = thu.ailab.utils.Tools.getTestFiles.slice(0, 
        MyConfigFactory.getValue[Int](dataset, "template.extractCount"))
    val templateArray = TemplateManager.recoverTemplates()
    val sb = new StringBuilder
    for (fn <- filenames) {
      val vtnArray = new TreeBuilder(fn).getVerboseTagSequence.toArray
      val thatTagSeq = TagSequence.fromNodeArray(vtnArray, false)
      val tpOption = templateArray.chooseTemplate(thatTagSeq)
      /**
       * If we find an appropriate template, build output
       */
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
    withPrintWriter(MyConfigFactory.getValue[String](dataset, "template.extractResult")) { pw =>
      pw.println("<documents>")
      pw.println(sb)
      pw.println("</documents>")
    }
  }
  def testBuild() {
    println(TemplateManager.buildTemplates)
  }
  /**
   * Choose a function by the configuration
   */
  MyConfigFactory.getValue[String]("global.stage") match {
    case "build" => testBuild()
    case "extractRandom" => testExtract()
    case "extractAll" => testSaveToXML()
  }  
}
