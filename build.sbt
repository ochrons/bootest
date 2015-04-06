// Turn this project into a Scala.js project by importing these settings
enablePlugins(ScalaJSPlugin)

name := "BooTest"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.6"

persistLauncher in Compile := true

persistLauncher in Test := false

testFrameworks += new TestFramework("utest.runner.Framework")

bootSnippet := "BooApp().main();"

workbenchSettings

libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.8.0",
    "com.lihaoyi" %%% "upickle" % "0.2.8",
    "com.lihaoyi" %%% "scalatags" % "0.4.6",
    "com.lihaoyi" %%% "utest" % "0.3.0" % "test"
)
