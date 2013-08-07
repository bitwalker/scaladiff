import sbt._
import Keys._

object Builds extends Build {
  lazy val buildSettings = Defaults.defaultSettings ++ Seq(
    version := "0.1",
    organization := "net.ironforged",
    scalaVersion := "2.10.2"
  )
}
