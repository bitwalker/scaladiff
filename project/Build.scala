import sbt._
import Keys._

object Builds extends Build {

  addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8")

  lazy val buildSettings = Defaults.defaultSettings ++ Seq(
    version := "0.1-SNAPSHOT",
    name := "scaladiff",
    organization := "net.ironforged",
    scalaVersion := "2.10.2",
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    libraryDependencies ++= Seq(
        "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
        "com.novocode" % "junit-interface" % "0.10" % "test"
    ),
    publishMavenStyle := true,
    publishTo <<= version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    publishArtifact in Test := false,
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    pomExtra := (
      <url>https://github.com/bitwalker/scaladiff</url>
        <licenses>
          <license>
            <name>Apache License 2.0</name>
            <url>http://www.apache.org/licenses/LICENSE-2.0</url>
            <distribution>repo</distribution>
          </license>
        </licenses>
        <scm>
          <url>git@github.com:bitwalker/scaladiff.git</url>
          <connection>scm:git:git@github.com:bitwalker/scaladiff.git</connection>
        </scm>
        <developers>
          <developer>
            <id>bitwalker</id>
            <name>Paul Schoenfelder</name>
            <url>http://bitwalker.github.io</url>
          </developer>
        </developers>)
  )

  lazy val app = Project(
    "scaladiff",
    file("."),
    settings = buildSettings
  ) settings(
    // placeholder
  )
}