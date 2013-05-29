import com.typesafe.sbt.SbtStartScript

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"

libraryDependencies += "com.twitter" % "util-core_2.10" % "6.3.4"

libraryDependencies += "com.twitter" % "util-logging_2.10" % "6.3.4"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies +=
"com.typesafe.akka" %% "akka-actor" % "2.1.4"

seq(SbtStartScript.startScriptForClassesSettings: _*)

name := "TemplateExtractor"

scalaVersion in ThisBuild := "2.10.1"

scalacOptions ++= Seq("-deprecation", "-unchecked")

unmanagedResourceDirectories in Compile <+= baseDirectory{ _ / "conf"}

mainClass in (Compile, run) := Some("thu.ailab.document.TestPreprocess")
