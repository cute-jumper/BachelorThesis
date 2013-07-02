resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"

libraryDependencies += "com.twitter" % "util-core_2.10" % "6.3.4"

libraryDependencies += "com.twitter" % "util-logging_2.10" % "6.3.4"

libraryDependencies +=
"com.typesafe.akka" %% "akka-actor" % "2.1.4"

libraryDependencies += "org.jsoup" % "jsoup" % "1.7.2"

libraryDependencies += "com.ibm.icu" % "icu4j" % "51.1"

name := "TemplateExtractor"

scalaVersion in ThisBuild := "2.10.1"

scalacOptions ++= Seq("-deprecation", "-unchecked")

unmanagedResourceDirectories in Compile <+= baseDirectory{ _ / "conf"}

mainClass in (Compile, run) := Some("thu.ailab.document.TestPreprocess")
