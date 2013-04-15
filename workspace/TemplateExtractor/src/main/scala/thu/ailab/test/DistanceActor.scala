package thu.ailab.test

import akka.actor._
import akka.routing.RoundRobinRouter
import akka.util.Duration
import akka.util.duration._

object TextSimilarities {
  object TriangleType extends Enumeration {
    type TriangleType = Value
    val DOWN, UP = Value
  }
  import TriangleType._
  sealed trait Message
  case class TriangleSplit(left: Int, right: Int, triangleType: TriangleType) extends Message
  case class Finished(total: Int, dists: Map[Tuple2[Int, Int], Double]) extends Message
  
  val docFactory = new 
} 