import sbt._
import Keys._

//import sbtrelease.ReleasePlugin._

object MinimalBuild extends Build {
  val SNAPSHOT = "-SNAPSHOT"

  lazy val buildVersion =  "0.0.5" + SNAPSHOT

  lazy val typesafeSnapshot = "Typesafe Snapshots Repository" at "http://repo.typesafe.com/typesafe/snapshots/"
  lazy val typesafe = "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

  val scalazVersionNumber = "7.0.0-M7"
  val scalaz = "org.scalaz" %% "scalaz-core" % scalazVersionNumber

  val akkaz = "akkaz" %% "akkaz" % "0.0.1"


  val neo4jVersion                  = "1.9.M03"
  lazy val neo4j                    = "org.neo4j.app" % "neo4j-server" % neo4jVersion % "test" classifier "static-web" classifier ""
  lazy val neo4jkernel              = "org.neo4j" % "neo4j-kernel" % neo4jVersion % "test"  classifier "tests" classifier ""
  lazy val jerseyForNeo4J           = "com.sun.jersey" % "jersey-core" % "1.9" % "test"

  val specs2 = "org.specs2" %% "specs2" % "1.12.3" % "test"

  val libDependencies = Seq(
    "play" %% "play" % "2.1-RC1",

    scalaz,
    akkaz,

    neo4j,
    neo4jkernel,
    jerseyForNeo4J,

    specs2,
    "play" %% "play-test" % "2.1-RC1" % "test"
  )

  lazy val root =
    Project(
      id = "neo4j-rest-play-plugin",
      base = file("."),
      settings = Project.defaultSettings// ++ releaseSettings
    ).settings(
      scalaVersion := "2.10.0",

      version := buildVersion,
      organization := "be.nextlab",

      //resolvers ++= Seq(typesafe, typesafeSnapshot),//, cloudbeesRepo),

      javacOptions += "-Xlint:unchecked",
      libraryDependencies ++= libDependencies,

      licenses := Seq("Apache License, Version 2.0" -> url("http://opensource.org/licenses/apache2.0.php")),



      publishMavenStyle := true,
      publishTo <<= version { (v: String) =>
                      val nexus = "https://oss.sonatype.org/"
                      if (v.trim.endsWith("SNAPSHOT"))
                        Some("snapshots" at nexus + "content/repositories/snapshots")
                      else
                        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
                    },

      credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),

      publishArtifact in Test := false,
      pomIncludeRepository := { _ => false },

      pomExtra := (
        <url>https://github.com/andypetrella/neo4j-rest-play-plugin</url>
        <scm>
          <url>git@github.com:andypetrella/neo4j-rest-play-plugin.git</url>
          <connection>scm:git:git@github.com:andypetrella/neo4j-rest-play-plugin.git</connection>
        </scm>
        <developers>
          <developer>
            <id>noootsab</id>
            <name>Andy Petrella</name>
            <url>http://ska-la.blogspot.be/</url>
          </developer>
        </developers>)
    )
}