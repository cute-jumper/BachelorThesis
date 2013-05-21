package thu.ailab.template

import thu.ailab.tree.TreeNode
import thu.ailab.factory.MyConfigFactory

class TemplateBuilder {
  def ClusterFileReader() = {
    val clusterXML = xml.XML.loadFile(
        MyConfigFactory.getValue[String]("output.clusterFile"))
    for (c <- clusterXML \ "cluster") yield c \ "point" map (_.text)
  }
  val clusterFilenames = ClusterFileReader()
  getClusterTemplate(clusterFilenames(0))
  def getClusterTemplate(filenames: Seq[String]) = {
    
  }
  
}

object TestTemplateBuilder extends App {
  
}