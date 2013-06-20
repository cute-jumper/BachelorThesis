package thu.ailab.sequence

import java.io.File

import thu.ailab.global.MyConfigFactory
import thu.ailab.tree.{TreeNode, TreeBuilder, HTMLSuffixTree}

/**
 * Manage all the TagSequences. Each TagSequence stands for a document.
 */
class TagSeqFactory(id2filename: Array[String]) {
  final val factorySize = id2filename.length
  def getSize() = factorySize
  def getFilename(id: Int) = id2filename(id)
  val documentCache = new Array[TagSequence](factorySize)
  private def fileIsLoaded(id: Int): Boolean = documentCache(id) != null
  
  private val dataset = MyConfigFactory.getValue[String]("global.dataset")
  private val docDir = MyConfigFactory.getValue[String](dataset, "document.dir")
  private val prepDir = MyConfigFactory.getValue[String](dataset, "preprocess.dir")
  
  def getPrepFilename(filename: String) = filename.replace(docDir, prepDir) 
  
  /**
   * If we have pre-processed the document, use the pre-processing result,
   * else use the original document.
   */
  def getInstance(id: Int) = {
    if (documentCache(id) == null) {
      val filename = id2filename(id)
      val prepFilename = getPrepFilename(filename)
      val tagArray: TagSequence = {
        if (new File(prepFilename).exists) {
          val tagSeqXML = scala.xml.XML.loadFile(prepFilename)
          TagSequence.fromXML(tagSeqXML)
        } else {
          TagSequence.fromNodeArray(new TreeBuilder(filename).getTagSequence.toArray, false)
        }
      }
      documentCache(id) = tagArray
    }
    documentCache(id)
  }
  
  def getAllInstances() = {
    (0 until factorySize).toArray map getInstance
  }
}

