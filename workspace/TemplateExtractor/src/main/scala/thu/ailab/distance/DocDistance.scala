package thu.ailab.distance

import thu.ailab.sequence.TagSeqFactory
import thu.ailab.global.MyConfigFactory
import thu.ailab.global.AppEntry

object DocDistance extends {
  val id2filename = new java.io.File( 
      MyConfigFactory.getValue[String]("document.blogdir")).listFiles().map { 
    _.getAbsolutePath
  }
  val tagSeqFactory = new TagSeqFactory(id2filename)  
  override val totalSize = tagSeqFactory.getSize
} with CalcDistance {
  /**
   * overrides
   */
  override def getSequence(id: Int) = tagSeqFactory.getInstance(id)
  override def postCalculation() {
    import thu.ailab.utils.Tools.withPrintWriter    
    withPrintWriter(MyConfigFactory.getValue[String]("output.distFile")) { pw =>
      var idx = 0
      for (i <- 0 until totalSize; j <- 0 until i) {
        pw.println("%f".format(distArray(idx)))
        idx += 1
      }
    }
    withPrintWriter(MyConfigFactory.getValue[String]("output.id2filename")) {
      pw => id2filename.foreach(pw.println)
    }
  }  
}

object TestDocDistance extends AppEntry {
  /**
   * DO ALL THE WORK!
   */
  DocDistance.doCalculation()
  print("Hello World")
}