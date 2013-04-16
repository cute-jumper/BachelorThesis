package thu.ailab.test

import akka.actor._
import akka.routing.RoundRobinRouter
import akka.util.Duration
import akka.util.duration._
import thu.ailab.tree.TagSeqFactory
import thu.ailab.config._
import thu.ailab.utils.Point
import thu.ailab.distance.LCSAlgo
import thu.ailab.tree.TreeNode
import thu.ailab.global.AppEntry

object TextSimilarities extends AppEntry {
  object TriangleType extends Enumeration {
    type TriangleType = Value
    val DOWN, UP = Value
  }
  import TriangleType._
  sealed trait Message
  case object StartCalculation extends Message
  case class AreaSplit(p1: Point, p2: Point) extends Message
  case class AreaFinished() extends Message
  case class AllFinished(duration: Duration) extends Message
  
  val fdirConfig = (MyConfigFactory.get("MyFileDirectoriesConfig").get).asInstanceOf[MyFileDirectoriesConfig]
  val outputFilesConfig = MyConfigFactory.get("MyOutputFilesConfig").get.asInstanceOf[MyOutputFilesConfig]
  
  val factory = new TagSeqFactory(fdirConfig.get("blogdir").get)
  val algoRunner = new LCSAlgo[TreeNode]
      
  val distArray = new Array[Double](factory.size * (factory.size - 1) / 2)
  
  class Worker extends Actor {
    def receive = {
      case triangleSplit @ AreaSplit(p1, p2) =>
        calculateArea(p1, p2)
        sender ! AreaFinished
    }
    def calculateArea(p1: Point, p2: Point) = {
      for (i <- p1.x + 1 to p2.x; j <- p1.y until math.max(p2.y, i)) {
        distArray((i - 1) * i / 2 + j) = algoRunner.run(factory.getInstance(i), factory.getInstance(j))
      }
    }
  }
  class Master(nrOfWorkers: Int,
      pieceLength: Int,
      listener: ActorRef) extends Actor {
    val start = System.currentTimeMillis()
    val workerRouter = context.actorOf(
      Props[Worker].withRouter(RoundRobinRouter(nrOfWorkers)), name = "workRouter")
    val nrOfHSplit = (factory.size - 1) / pieceLength + 1
    val nrOfMessages = (nrOfHSplit + 1) * nrOfHSplit / 2
    var finishedCount = 0
    def receive = {
      case StartCalculation =>
        for (i <- 0 until nrOfHSplit - 1; j <- 0 to i)
          workerRouter ! AreaSplit(Point(i * pieceLength, j * pieceLength), 
              Point(math.min((i + 1) * pieceLength, factory.size),
                  math.min((j + 1) * pieceLength, factory.size)))
      case AreaFinished => 
        finishedCount += 1
        if (finishedCount == nrOfMessages) {
          listener ! AllFinished((System.currentTimeMillis() - start).millis)
          context.stop(self)
        }
    }
  }
  
  class Listener extends Actor {
    def receive = {
      case AllFinished(duration) =>
        writeFile(outputFilesConfig.get("distancesFile").get)
        println("Calculation time: %s".format(duration))
        context.system.shutdown
    }
    import thu.ailab.utils.Tools.withPrintWriter
    def writeFile(filename: String) = {
      withPrintWriter(filename) {pw =>
        var idx = 0
        for (i <- 1 until factory.size; j <- 0 until i) {
          pw.println("%s\t%s\t%f".format(
              factory.getFilename(i), factory.getFilename(j), distArray(idx)))
          idx += 1
        }
      }
    }
  }
  
  def calculate(nrOfWorkers: Int, pieceLength: Int) = {
    val system = ActorSystem("CalcSimilarities")
    val listener = system.actorOf(Props[Listener], name = "listener")
    val master = system.actorOf(Props(new Master(
        nrOfWorkers, pieceLength, listener)),
        name = "master")
     master ! StartCalculation 
  }
  
  calculate(nrOfWorkers = 100, pieceLength = 100)
}