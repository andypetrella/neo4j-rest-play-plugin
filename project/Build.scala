import sbt._
import Keys._


object MinimalBuild extends Build {
  val SNAPSHOT = "-SNAPSHOT"

  lazy val buildVersion =  "0.0.3" + SNAPSHOT

  lazy val typesafeSnapshot = "Typesafe Snapshots Repository" at "http://repo.typesafe.com/typesafe/snapshots/"
  lazy val typesafe = "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

  val scalazVersionNumber = "6.0.4"
  val scalaz = "org.scalaz" %% "scalaz-core" % scalazVersionNumber withSources

  lazy val neo4j                   = "org.neo4j.app" % "neo4j-server" % "1.9.M01" % "test" classifier "static-web" classifier ""
  lazy val neo4jkernel             = "org.neo4j" % "neo4j-kernel" % "1.9.M01" % "test"  classifier "tests" classifier ""
  lazy val jerseyForNeo4J          = "com.sun.jersey" % "jersey-core" % "1.9" % "test"

  val specs2 = "org.specs2" %% "specs2" % "1.12.2" % "test" withSources

  val cloudbees = "https://repository-andy-petrella.forge.cloudbees.com/"
  val cloudbeesRepo = buildVersion match {
    case x if x.endsWith(SNAPSHOT) => x.toLowerCase at cloudbees + "snapshot" + "/"
    case x => x.toLowerCase at cloudbees + "release" + "/"
  }

  val libDependencies = Seq(
    "play" %% "play" % "2.0.3",

    scalaz,

    neo4j,
    neo4jkernel,
    jerseyForNeo4J,
    specs2,
    "play" %% "play-test" % "2.0.3" % "test"
  )

  private def timestamp(tsFormat:String): String = {
    import java.{util => ju}
    val sf = new java.text.SimpleDateFormat(tsFormat)
    sf.setTimeZone(ju.TimeZone.getTimeZone("UTC"))
    sf.format(new ju.Date())
  }


  val cloudbeesCredentials = Credentials(file("project/cloudbees.credentials"))
  lazy val root = {
    Project(id = "neo4j-rest-play-plugin", base = file("."), settings = Project.defaultSettings).settings(
      version := buildVersion,
      organization := "be.nextlab",
      resolvers ++= Seq(typesafe, typesafeSnapshot, cloudbeesRepo),
      javacOptions += "-Xlint:unchecked",
      libraryDependencies ++= libDependencies,
      publishMavenStyle := true,
      publishTo := Some(cloudbeesRepo),
      credentials += cloudbeesCredentials,
      version <<= (version) { v =>
          v + "-" + timestamp("yyyyMMdd")
      }
    )
  }
}