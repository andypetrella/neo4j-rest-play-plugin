package be.nextlab.play.neo4j.rest

import org.specs2.Specification
import play.api.libs.concurrent.Promise
import play.api.test._
import play.api.test.Helpers._
import Neo4JTestHelpers._
import scalaz.{Success => OK, Failure => KO, _}
import scalaz.Scalaz._
import Neo4JElement._

import play.api.Play.current

import play.api.libs.concurrent.Akka
import scala.concurrent.Promise
import scala.concurrent.Future

//Akkaz: implementation of Functor[Future] and Monad[Future]
import scalaz.akkaz.future._

/**
 *
 * User: noootsab
 * Date: 1/04/12
 * Time: 14:34
 */

object RootTest extends Specification {
  implicit lazy val executionContext = Akka.system.dispatcher

  def is = "Test service root" ^ {

    "Get it" ! neoApp {
      await(endPoint.root) must be like {
        case r:Root => r.neo4jVersion must be_=== (plugin.neo4jVersion)
      }
    }

  }

}