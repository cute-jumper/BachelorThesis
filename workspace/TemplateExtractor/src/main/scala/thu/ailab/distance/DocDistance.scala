package thu.ailab.distance

import thu.ailab.sequence.TagSeqFactory
import thu.ailab.global.MyConfigFactory
import thu.ailab.global.AppEntry

object DocDistance extends {
  val dataset = MyConfigFactory.getValue[String]("global.dataset")
  val id2filename = new java.io.File( 
      MyConfigFactory.getValue[String](dataset, "document.dir")).listFiles().map { 
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
    withPrintWriter(MyConfigFactory.getValue[String](dataset, "output.distFile")) { pw =>
      var idx = 0
      for (i <- 0 until totalSize; j <- 0 until i) {
        pw.println("%f".format(distArray(idx)))
        idx += 1
      }
    }
    withPrintWriter(MyConfigFactory.getValue[String](dataset, "output.id2filename")) {
      pw => id2filename.foreach(pw.println)
    }
  }  
}

object TestDocDistance extends AppEntry {
  /**
   * DO ALL THE WORK!
   */
  DocDistance.doCalculation()
}