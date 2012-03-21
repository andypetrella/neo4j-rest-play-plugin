import sbt._
import Keys._

object MinimalBuild extends Build {

  lazy val buildVersion =  "1.0-SNAPSHOT"

  lazy val typesafeSnapshot = "Typesafe Snapshots Repository" at "http://repo.typesafe.com/typesafe/snapshots/"
  lazy val typesafe = "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
  lazy val repo = if (buildVersion.endsWith("SNAPSHOT")) typesafeSnapshot else typesafe

  val libDependencies = Seq(
    "play" %% "play" % "2.0",
    "net.databinder" %% "dispatch-http" % "0.8.7" withSources
  )



  lazy val root = Project(id = "neo4j-rest-play-plugin", base = file("."), settings = Project.defaultSettings).settings(
    version := buildVersion,
    organization := "be.nextlab",
    resolvers += repo,
    javacOptions += "-Xlint:unchecked",
    libraryDependencies ++= libDependencies
  )
}