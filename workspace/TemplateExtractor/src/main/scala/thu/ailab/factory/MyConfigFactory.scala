package thu.ailab.factory

import com.typesafe.config._
import scala.collection.mutable.HashMap
import java.io.File

object MyConfigFactory {
  private val conf = ConfigFactory.load()
  private def getConfString(path: String) = {
    /**
     * Replace the `~' with user's home directory
     */
    def simpleExpansion(filepath: String) = 
      if (filepath.startsWith("~" + File.separator)) 
        System.getProperty("user.home") + filepath.substring(1)
      else 
        filepath
    simpleExpansion(conf.getString(path))
  }
  def hasValue(name: String) = conf.hasPath(name)
  /**
   * A generic method using Manifest
   */
  def getValue[T](name: String)(implicit mf: Manifest[T]): T = {
    val dispatch = Map(
        classOf[String] -> getConfString _,
        classOf[Double] -> conf.getDouble _,
        classOf[Int] -> conf.getInt _
        )
    //(dispatch.find(_._1 == mf.erasure).map(_._2).get)(name).asInstanceOf[T]
    (dispatch.find(_._1 isAssignableFrom mf.erasure).map(_._2).get)(name).asInstanceOf[T]
  }
  def getValue1[T: Manifest](name: String): T = {
    ((manifest[T].toString match { // or manifest[T].erasure.getName match {
      case "java.lang.String" => getConfString _
      case "Int" => conf.getInt _
      case "Double" => conf.getDouble _
    })(name)).asInstanceOf[T]
  }
}

object TestMyConfigFactory extends App {
  println(MyConfigFactory.getValue1[String]("output.distFile"))
  println(MyConfigFactory.getValue1[Int]("actor.nrOfWorkers") + 2)
}
