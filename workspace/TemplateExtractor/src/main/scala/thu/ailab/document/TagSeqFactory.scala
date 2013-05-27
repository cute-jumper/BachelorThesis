package thu.ailab.document

import java.io.File

import thu.ailab.global.MyConfigFactory
import thu.ailab.tree.{TreeNode, TreeBuilder, HTMLSuffixTree}

class TagSeqFactory(id2filename: Array[String]) {
  final val factorySize = id2filename.length
  def getSize() = factorySize
  def getFilename(id: Int) = id2filename(id)
  val documentCache = new Array[TagSequence](factorySize)
  private def fileIsLoaded(id: Int): Boolean = documentCache(id) != null
  
  private val blogdir = MyConfigFactory.getValue[String]("document.blogdir")
  private val prepBlogdir = MyConfigFactory.getValue[String]("preprocess.blogdir")
  
  def getPrepFilename(filename: String) = filename.replace(blogdir, prepBlogdir) 
  
  def getInstance(id: Int) = {
    if (documentCache(id) == null) {
      val filename = id2filename(id)
      val prepFilename = getPrepFilename(filename)
      val tagArray: TagSequence = {
        if (new File(prepFilename).exists)
          new TagSequence(Preprocess.readCompactNodeArray(prepFilename), true)
        else
          new TagSequence(new TreeBuilder(id2filename(id)).getTagSequence.toArray, false)
      }
      documentCache(id) = tagArray
    }
    documentCache(id)
  }
  
  def getAllInstances() = {
    (0 until factorySize).toArray map getInstance
  }
}
