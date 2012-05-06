package be.nextlab.play.neo4j.rest

import org.specs2.Specification
import play.api.test._
import play.api.test.Helpers._
import Neo4JTestHelpers._
import Neo4JElement._
import play.api.libs.json.{JsValue, JsNumber, JsString, JsObject}

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
        await(for (
          root <- endPoint.root;
          ref <- root.referenceNode;
          newNode <- root.createNode(None);
          rel <- ref.asInstanceOf[Node].createRelationship(Relation(
            Right(ref.asInstanceOf[Node]),
            Right(newNode.asInstanceOf[Node]),
            "TEST",
            Nil)
          )
        ) yield (ref, rel, newNode)) match {

          case (ref: Node, r: Relation, newNode: Node) => (r.self must be_!=("")) and (r.`type` must be_==("TEST")) and (r.end.await.get must be_==(newNode)) and (r.start.await.get must be_==(ref))
          case _ => ko("bad match")

        }
      } ^
        "Get outgoing relation of the Reference Node" ! neoApp {
          await(for (
            root <- endPoint.root;
            ref <- root.referenceNode;
            newNode <- root.createNode(None);
            rel <- ref.asInstanceOf[Node].createRelationship(Relation(
              Right(ref.asInstanceOf[Node]),
              Right(newNode.asInstanceOf[Node]),
              "TEST", Nil)
            );
            rels <- ref.asInstanceOf[Node].outgoingTypedRelationships(Seq("TEST"))
          ) yield (ref, rel, rels)) match {

            case (ref: Node, r: Relation, rs: Seq[Relation]) => rs match {
              case Nil => ko("Must return at least one Relation")
              case xs => xs must contain(r)
            }
            case _ => ko("bad match")

          }

        }

    }

  }

}
