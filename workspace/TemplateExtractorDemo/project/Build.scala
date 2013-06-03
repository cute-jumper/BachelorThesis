import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "TemplateExtractorDemo"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm,
    "org.jsoup" % "jsoup" % "1.7.2"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
  )

}
