package be.nextlab.play.neo4j.rest

import org.specs2.Specification
import play.api.test._
import play.api.test.Helpers._
import Neo4JTestHelpers._
import Neo4JElement._
import play.api.libs.json.{JsValue, JsNumber, JsString, JsObject}
import scalaz.{Success => OK, Failure => KO, _}
import scalaz.Scalaz._

import play.api.Play.current

import play.api.libs.concurrent.Akka
import scala.concurrent.Promise
import scala.concurrent.Future

//Akkaz: implementation of Functor[Future] and Monad[Future]
//import scalaz.akkaz.future._

/**
 *
 * User: noootsab
 * Date: 1/04/12
 * Time: 10:41
 */

object RelationTest extends Specification {
  implicit lazy val executionContext = Akka.system.dispatcher

  def rnds = BigInt(100, scala.util.Random).toString(36)

  def is = "Deal with relations in Neo4J using the REST api " ^ {

    "Based on Node " ^ {

      "Create outgoing of the Reference Node" ! neoApp {
        await(for {
          r <- endPoint.root;
          ref <- r.referenceNode;
          newNode <- r.createNode(None);
          rel <- ref.createRelationship(Relation(
                  Right(ref),
                  Right(newNode),
                  "TEST",
                  Nil)
                )
          } yield (ref, rel, newNode)
        ) match {
          case ((ref: Node), (r: Relation), (newNode: Node)) =>
            (r.self must be_!=("")) and
              (r.rtype must be_==("TEST")) and
              (await(r.end) must be like {
                case n if n == newNode => ok("got the good end")
                case _ => ko("unable to retrieve the end node")
              }) and
              (await(r.start) must be like {
                case n if n == ref => ok("got the good start")
                case _ => ko("unable to retrieve the start node")
              })
          case _ => ko("bad match")

        }
      } ^
        "Get outgoing relation of the Reference Node" ! neoApp {
          await(for {
            r <- endPoint.root;
            ref <- r.referenceNode;
            newNode <- r.createNode(None);
            rel <- ref.createRelationship(Relation(
                    Right(ref),
                    Right(newNode),
                    "TEST",
                    Nil)
                  );
            rels <- ref.outgoingTypedRelationships(Seq("TEST"))
            } yield (ref, rel, rels)
          ) match {

            case ((ref: Node), (r: Relation), (rs: Seq[Relation])) => rs match {
              case Nil => ko("Must return at least one Relation")
              case xs => xs must haveOneElementLike {case i:Relation => (i.rtype must be_==("TEST")) and (i.id must be_==(r.id))}
            }
            case _ => ko("bad match")
          }
        }
    }
  }
}
