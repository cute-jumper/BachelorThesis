package thu.ailab.test

import thu.ailab.global._
import thu.ailab.template.TemplateManager
import thu.ailab.template.EssentialNode
import thu.ailab.template.OptionalNode

/**
 * Test code should put here 
 */
object Test extends AppEntry {
  def getTemplateStat(templateFile: String) = {
    val tpMan = TemplateManager.recoverTemplates(templateFile)
    var enLength, enCount, onCount = 0
    var onLength = 0.0
    for (tp <- tpMan.getTemplates; tn <- tp.tnArray) {
      tn match {
        case en: EssentialNode => enLength += en.getTreeNodeCount; enCount += 1
        case on: OptionalNode => onLength += on.getAverageLength; onCount += 1
      }
    }
    println("template average: " + 1.0 * 
        tpMan.getTemplates.map(_.tnArray.size).sum / tpMan.getTemplates.size) 
    println("en average: " + enLength * 1.0 / enCount)
    println("on average: " + onLength * 1.0 / onCount)
  }
  getTemplateStat(sys.props("user.home") + "/Programs/BachelorThesis/Data/material/templateFile_news_0.3")
  getTemplateStat(sys.props("user.home") + "/Programs/BachelorThesis/Data/material/templateFile_news_0.4")
  getTemplateStat(sys.props("user.home") + "/Programs/BachelorThesis/Data/material/templateFile_news_0.5")
  getTemplateStat(sys.props("user.home") + "/Programs/BachelorThesis/Data/material/templateFile_news_0.6")
  
  def rebuildTemplates() = {
    val dataset = MyConfigFactory.getValue[String]("global.dataset")
    val templateFile = MyConfigFactory.getValue[String](dataset, "template.templateFile")
    val templatesXML = scala.xml.XML.loadFile(templateFile)
    val templates = (templatesXML \ "template" map (thu.ailab.template.Template.fromOldXML(_)))
    scala.xml.XML.save(templateFile + "_result",
      <templates>
        {
          for (tp <- templates) yield (tp.toXML)
        }
      </templates>)
  }
  //rebuildTemplates()
}
