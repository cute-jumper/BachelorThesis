package thu.ailab.distance

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.concurrent.duration._
import akka.event.Logging
import thu.ailab.sequence.{TagSequence, TagSeqFactory}
import thu.ailab.distance._
import thu.ailab.tree.TreeNode
import thu.ailab.global._
import thu.ailab.utils.Tools.timeIt

/**
 * Class stand for a point
 */
class Point(val x: Int, val y: Int) extends Tuple2[Int, Int](x, y)

object Point {
  def apply(x: Int, y: Int) = new Point(x, y)
}

/**
 * This is a class, standing for a model which
 * uses "Actor" to calculate the distances between 
 * every two points.
 * 
 * One who extends the class should provide three things:
 * - totalSize: the total amount of points.
 * - getSequence: how to get TagSequence according to a id.
 * - postCalculation: what to do after a calculation.
 */
abstract class CalcDistance extends LoggerTrait {
  /**
   * Messages
   */
  sealed trait Message
  case object StartCalculation extends Message
  case class AreaSplit(p1: Point, p2: Point) extends Message
  case class AreaFinished() extends Message
  case class AllFinished(duration: Duration) extends Message  
  /**
   * Abstract fields and methods
   */
  val totalSize: Int
  def getSequence(id: Int): TagSequence
  def postCalculation(): Unit
  /**
   * Initialize distArray to store all distances.
   * Use 1-dimension array instead of 2-dimension array to save space.
   */
  val distArray = new Array[Double](totalSize * (totalSize - 1) / 2)
  
  /**
   * Main function for the outside callers. 
   */
  def doCalculation() = {
    setupAndRun(nrOfWorkers = MyConfigFactory.getValue[Int]("actor.nrOfWorkers"), 
        pieceLength = MyConfigFactory.getValue[Int]("actor.pieceLength"))
    distArray
  }

  /**
   * Given id1 and id2, calculate the distance between them.
   */
  def calcDistance(id1: Int, id2: Int) = {
    new LCSArraySpaceOptimized(getSequence(id1), getSequence(id2)).getDistance()
  }
  /**
   * Actors actually do calculation
   */
  class Worker extends Actor {
    def receive = {
      case AreaSplit(p1, p2) =>
        val (count, duration) = timeIt(calculateArea(p1, p2))
        sender ! AreaFinished
    }
    def calculateArea(p1: Point, p2: Point) = {
      var acc = 0 
      for (i <- p1.x + 1 to p2.x; j <- p1.y until math.min(p2.y, i)) {
        distArray((i - 1) * i / 2 + j) = calcDistance(i, j)
        acc += 1
      }
      acc
    }
  }
  /**
   * Control Actor, responsible for dispatching tasks
   */
  class Master(nrOfWorkers: Int,
      pieceLength: Int,
      listener: ActorRef) extends Actor {
    val start = System.currentTimeMillis()
    val workerRouter = context.actorOf(
      Props(new Worker).withRouter(RoundRobinRouter(nrOfWorkers)), 
      name = "workRouter")
    val nrOfHSplit = (totalSize - 2) / pieceLength + 1
    val nrOfMessages = (nrOfHSplit + 1) * nrOfHSplit / 2
    var finishedCount = 0
    def receive = {
      case StartCalculation =>
        var acc = 1
        for (i <- 0 until nrOfHSplit; j <- 0 to i) {
          acc += 1
          workerRouter ! AreaSplit(Point(i * pieceLength, j * pieceLength), 
              Point(math.min((i + 1) * pieceLength, totalSize - 1),
                  math.min((j + 1) * pieceLength, totalSize - 1)))
        }
      case AreaFinished => 
        finishedCount += 1
        if (finishedCount == nrOfMessages) {
          listener ! AllFinished((System.currentTimeMillis() - start).millis)
          context.stop(self)
        }
    }
  }
  /**
   * Listener, responsible for shut down the system
   */
  class Listener extends Actor {
    def receive = {
      case AllFinished(duration) =>
        logger.info("Calculation time: %s".format(duration))
        postCalculation()
        context.system.shutdown
    }
  }
  /**
   * Set up the system and run
   */
  private def setupAndRun(nrOfWorkers: Int, pieceLength: Int) = {
    val system = ActorSystem("CalcSimilarities")
    val listener = system.actorOf(Props(new Listener), name = "listener")
    val master = system.actorOf(Props(new Master(
        nrOfWorkers, pieceLength, listener)),
        name = "master")
    master ! StartCalculation
    system.awaitTermination
  }
  
}