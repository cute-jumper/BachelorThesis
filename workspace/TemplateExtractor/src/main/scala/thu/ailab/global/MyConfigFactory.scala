package thu.ailab.global

import com.typesafe.config._
import scala.collection.mutable.HashMap
import java.io.File

/**
 * A wrapper class to read configuration.
 */
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
   * A generic method using ClassTag.
   * 
   * ATTENTION: In Scala 2.10, we should use ClassTag instead of ClassManifest.
   * The latter is deprecated since 2.10.  
   */
  import scala.reflect.ClassTag
  def getValue[T](names: String*)(implicit ct: ClassTag[T]): T = {
    val name = names.mkString(".")
    val dispatch = Map(
        classOf[String] -> getConfString _,
        classOf[Double] -> conf.getDouble _,
        classOf[Int] -> conf.getInt _,
        classOf[Long] -> conf.getLong _
        )
    (dispatch.find(_._1 isAssignableFrom ct.runtimeClass).map(_._2).get)(name).asInstanceOf[T]
  }
  /** 
   * Another implementation which is more dirty. Only for fun.
   */
  private def getValue1[T: Manifest](name: String): T = {
    ((manifest[T].toString match { // or manifest[T].erasure.getName match {
      case "java.lang.String" => getConfString _
      case "Int" => conf.getInt _
      case "Double" => conf.getDouble _
    })(name)).asInstanceOf[T]
  }
}

object TestMyConfigFactory extends App {
  println(MyConfigFactory.getValue[String]("output.distFile"))
  println(MyConfigFactory.getValue[Int]("actor.nrOfWorkers") + 2)
}
