package thu.ailab.algo

trait Algorithm

class LCS[T <% Ordered[T]](val seq1: List[T], val seq2: List[T])  extends Algorithm {
  val len1 = seq1.length
  val len2 = seq2.length
  def recursiveCalcLCS = {
    def helper(seq1: List[T], seq2: List[T]): Int = {
      (seq1, seq2) match {
        case (Nil, _) => 0
        case (_, Nil) => 0
        case (x1::xs1, x2::xs2) =>
          if (x1 == x2) 1 + helper(xs1, xs2)
          else Math.max(helper(seq1, xs2), helper(xs1, seq2))
      }
    }
    helper(seq1, seq2)
  }
  def iterateCalcLCS: Int = {
    val table = Array.ofDim[Int](len1 + 1, len2 + 1)
    for (i <- 1 to seq1.length; j <- 1 to seq2.length) {
      if (seq1(i -  1) == seq2(j - 1))
        table(i)(j) = table(i - 1)(j - 1) + 1
      else 
        table(i)(j) = Math.max(table(i - 1)(j), table(i)(j - 1))
    }
    table(seq1.length)(seq2.length)
  }
  val lcs = iterateCalcLCS
//  val lcs = recursiveCalcLCS
  val distance = 1 - 1.0 * iterateCalcLCS / Math.max(len1, len2)
}

object TestLCS {
  def main(args: Array[String]) {
	println(new LCS("abcdefg".toList, "asdfasbdfasdfcsdfdsdfefdasfsdfg".toList).lcs)
  }
}