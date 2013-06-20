package thu.ailab.distance

import thu.ailab.sequence.TagSegment
import thu.ailab.global.AppEntry

/**
 * Class to calculation the distances between two TagSements
 */
class TSDistance(tss: Array[TagSegment]) extends {
  override val totalSize = tss.size
} with CalcDistance {
  /**
   * overrides
   */
  override def getSequence(id: Int) = tss(id).getTagSeq
  override def postCalculation() = {}
}

