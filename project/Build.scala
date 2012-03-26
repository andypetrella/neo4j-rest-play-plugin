import sbt._
import Keys._

object MinimalBuild extends Build {

  lazy val buildVersion =  "1.0-SNAPSHOT"

  lazy val typesafeSnapshot = "Typesafe Snapshots Repository" at "http://repo.typesafe.com/typesafe/snapshots/"
  lazy val typesafe = "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

  val scalazVersionNumber = "6.0.3"
  val scalaz = "org.scalaz" %% "scalaz-core" % scalazVersionNumber withSources

  val libDependencies = Seq(
    "play" %% "play" % "2.0",

    "net.databinder" %% "dispatch-http" % "0.8.7" withSources,

    scalaz
  )



  lazy val root = Project(id = "neo4j-rest-play-plugin", base = file("."), settings = Project.defaultSettings).settings(
    version := buildVersion,
    organization := "be.nextlab",
    resolvers ++= Seq(typesafe, typesafeSnapshot),
    javacOptions += "-Xlint:unchecked",
    libraryDependencies ++= libDependencies
  )
}