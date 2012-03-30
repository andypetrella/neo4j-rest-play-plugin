package be.nextlab.play.neo4j.rest

import play.Plugin
import play.api.{Play, Application}

/**
 * User: andy
 */

class Neo4JRestPlugin(app: Application) extends Plugin {

  lazy val protocol = app.configuration.getString("neo4j.rest.protocol").getOrElse("http")
  lazy val host = app.configuration.getString("neo4j.rest.host").getOrElse("localhost")
  lazy val port = app.configuration.getInt("neo4j.rest.port").getOrElse(7474)
  lazy val credentials = for (
    u <- app.configuration.getString("neo4j.rest.username");
    p <- app.configuration.getString("neo4j.rest.password")
  ) yield (u,  p)
    

  lazy val neo4j = Neo4JEndPoint(protocol, host, port, credentials)

  override def onStart() {

  }

}
