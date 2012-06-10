package be.nextlab.play.neo4j.rest

import org.specs2.Specification
import play.api.test._
import play.api.test.Helpers._
import Neo4JTestHelpers._
import Neo4JElement._
import play.api.libs.json.{JsValue, JsNumber, JsString, JsObject}
import play.api.libs.concurrent.Promise
import ValidationPromised._
import scalaz.{Success => OK, Failure => KO, _}
import scalaz.Scalaz._

/**
 *
 * User: noootsab
 * Date: 1/04/12
 * Time: 10:41
 */

object RelationTest extends Specification {

  def rnds = BigInt(100, scala.util.Random).toString(36)

  def is = "Deal with relations in Neo4J using the REST api " ^ {

    "Based on Node " ^ {

      "Create outgoing on Reference Node" ! neoApp {
        await(
          endPoint.root /~~>
          (root => 
            root.referenceNode /~~> (ref =>
              root.createNode(None) /~~>
              (newNode => 
                ref.createRelationship(Relation(
                  Right(ref),
                  Right(newNode),
                  "TEST",
                  Nil)
                ) /~~>
                (rel => Promise.pure(OK((ref, rel, newNode))))
              ) 
            )
          )
        ) match {

          case OK(((ref: Node), (r: Relation), (newNode: Node))) => 
            (r.self must be_!=("")) and 
              (r.`type` must be_==("TEST")) and 
              (await(r.end) must be like {
                case OK(n) if n == newNode => ok("got the good end")
                case _ => ko("unable to retrieve the end node")
              }) and 
              (await(r.start) must be like {
                case OK(n) if n == ref => ok("got the good start")
                case _ => ko("unable to retrieve the start node")
              })
          case _ => ko("bad match")

        }
      } ^
        "Get outgoing relation of the Reference Node" ! neoApp {
          await(
            endPoint.root /~~>
            (root => 
              root.referenceNode /~~> (ref =>
                root.createNode(None) /~~>
                (newNode => 
                  ref.createRelationship(Relation(
                    Right(ref),
                    Right(newNode),
                    "TEST",
                    Nil)
                  ) /~~> (
                    rel =>
                      ref.outgoingTypedRelationships(Seq("TEST"))  /~~> 
                       (rels => Promise.pure(OK((ref, rel, rels))))
                  )
                )
              )
            )
          ) match {

            case OK(((ref: Node), (r: Relation), (rs: Seq[Relation]))) => rs match {
              case Nil => ko("Must return at least one Relation")
              case xs => xs must contain(r)
            }
            case _ => ko("bad match")

          }

        }

    }

  }

}
