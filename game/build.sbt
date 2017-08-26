import org.scalajs.sbtplugin.ScalaJSPlugin

enablePlugins(ScalaJSPlugin)

name := "DungeonGenerator"

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.8.2"
)
    