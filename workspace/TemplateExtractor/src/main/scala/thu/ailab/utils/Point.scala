package thu.ailab.utils


/**
 * Class stand for a point
 */
class Point(val x: Int, val y: Int) extends Tuple2[Int, Int](x, y)

object Point {
  def apply(x: Int, y: Int) = new Point(x, y)
}
