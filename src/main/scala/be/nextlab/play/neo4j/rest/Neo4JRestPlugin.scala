package be.nextlab.play.neo4j.rest

import play.Plugin
import dispatch._
import play.api.libs.json._
import play.api.{Play, Application}

/**
 * User: andy
 */

class Neo4JRestPlugin(app: Application) extends Plugin {

  lazy val host = app.configuration.getString("neo4j.rest.host").getOrElse("localhost")
  lazy val port = app.configuration.getInt("neo4j.rest.port").getOrElse(7474)
  lazy val credentials = for (
    u <- app.configuration.getString("neo4j.rest.username");
    p <- app.configuration.getString("neo4j.rest.password")
  ) yield (u,  p)
    

  private lazy val restEndPointNoCredentials = :/(host, port)

  private[Neo4JRestPlugin] def resolveFrom(from:Either[String, Request]):Request = from match {
    case Left(s) => url(s)
    case Right(r) => r
  }
  
  def createRequest(from:Either[String, Request]) =
    credentials map {t:(String, String) => resolveFrom(from).as(t._1, t._2)} getOrElse resolveFrom(from)

  lazy val baseRestEndPoint = createRequest(Right(restEndPointNoCredentials))
    

  override def onStart() {

  }

}
