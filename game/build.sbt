import org.scalajs.sbtplugin.ScalaJSPlugin

enablePlugins(ScalaJSPlugin)

name := "DungeonGenerator"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.1",
  "org.scalaz" %% "scalaz-core" % "7.2.15",
  "com.softwaremill.quicklens" %%% "quicklens" % "1.4.8"
)
    