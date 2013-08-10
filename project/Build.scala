import sbt._
import Keys._

object Builds extends Build {

  lazy val buildSettings = Defaults.defaultSettings ++ Seq(
    version := "0.1",
    name := "scaladiff",
    organization := "net.ironforged",
    scalaVersion := "2.10.2",
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
  )


  lazy val app = Project(
    "scaladiff",
    file("."),
    settings = buildSettings
  ) settings(
    // placeholder
  )
}