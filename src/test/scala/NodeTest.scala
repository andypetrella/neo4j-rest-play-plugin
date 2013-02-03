package play.neo4j.rest

import org.specs2.Specification

import play.api.Play.current

import play.api.test._
import play.api.test.Helpers._

import be.nextlab.play.neo4j.rest.Neo4JTestHelpers._

import play.api.libs.concurrent.Akka
import scala.concurrent.Future

class NodeTest extends Specification {

  implicit lazy val executionContext = Akka.system.dispatcher

  lazy val service =
    Service19M04(
      Protocol(plugin.protocol),
      plugin.host,
      plugin.port,
      plugin.credentials
    )

  def is =

    "use service" ^ {
      "by id " ! neoApp {
        import service._
        await(service(Queries.byId[Node](0))) must beSome
      } ^
      "by id not exist" ! neoApp {
        import service._
        await(service(Queries.byId[Node](-1))) must beNone
      }
    }


}