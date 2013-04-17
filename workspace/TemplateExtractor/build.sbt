import com.typesafe.sbt.SbtStartScript

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"

seq(SbtStartScript.startScriptForClassesSettings: _*)

name := "TemplateExtractor"

scalacOptions += "-deprecation"

unmanagedSourceDirectories in Compile <+= baseDirectory{ _ / "conf"}

mainClass in (Compile, run) := Some("thu.ailab.test.TextSimilarities")