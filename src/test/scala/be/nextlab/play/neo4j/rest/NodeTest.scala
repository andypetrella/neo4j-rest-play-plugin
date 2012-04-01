package be.nextlab.play.neo4j.rest

import org.specs2.Specification
import play.api.test._
import play.api.test.Helpers._
import Neo4JTestHelpers._
import Neo4JElement._
import play.api.libs.json.{JsNumber, JsString, JsObject}

/**
 *
 * User: noootsab
 * Date: 1/04/12
 * Time: 10:41
 */

object NodeTest extends Specification {

  def is = "Deal with nodes in Neo4J using the REST api " ^ {
    "Get the reference Node " !
      neoApp {
        val root = endPoint.root
        await (root flatMap (_.referenceNode)) must be like {
          case n:Node => ok(" is ok since we got a Node")
          case x => ko(" is not ok because we didn't dot a Node, but " + x)
        }
      } ^
    "Create a new Empty Node " ! neoApp {
      val root = endPoint.root
      await (root flatMap (_.createNode(None))) must be like {
        case n:Node => ok(" is ok since we got a Node")
        case x => ko(" is not ok because we didn't dot a Node, but " + x)
      }
    } ^
    "Create a new Node w/ Properties " ! neoApp {
      val root = endPoint.root
      val node: Node = Node(JsObject(Seq("data" -> JsObject(Seq("a" -> JsString("valueForA"))))))
      await (root flatMap (_.createNode(Some(node)))) must be like {
        case n:Node => {
          (n.id must be_>= (0)) and (n.self must not beEmpty) and (n.data must be_== (node.data))
        }
        case x => ko(" is not ok because we didn't dot a Node, but " + x)
      }
    } ^
    "Update the data of Node w/ Properties " ! neoApp {
      val root = endPoint.root
      val node: Node = Node(JsObject(Seq("data" -> JsObject(Seq("a" -> JsString("valueForA"))))))
      val newProps: JsObject = JsObject(Seq("a" -> JsString("newValueForA"), "b" -> JsNumber(1)))
      var created:Option[Node] = None
      await (root flatMap (_.createNode(Some(node))) flatMap (c => {created = Some(c); c.properties(Some(newProps))})) must be like {
        case n:Node => {
          (n.id must be_== (created.get.id)) and (n.self must be_== (created.get.self)) and (n.data must be_== (newProps))
        }
        case x => ko(" is not ok because we didn't dot a Node, but " + x)
      }
    } ^
    "Delete a Node " ! neoApp {
      val root = endPoint.root
      val node: Node = Node(JsObject(Seq("data" -> JsObject(Seq("a" -> JsString("valueForA"))))))
      var created:Option[Node] = None
      await (root flatMap (_.createNode(Some(node))) flatMap (c => {created = Some(c); c.delete})) must be like {
        case n:Node => n must be_== (created.get) and  {
          await(root.flatMap(_.getNode(n.id))) must be like {
            case f:Failure => ok("Get the deleted must return a Failure instance")
            case x => ko("Get the deleted must return a Failure instance, got " + x)
          }
        }
        case x => ko(" is not ok because we didn't dot a Node, but " + x)
      }
    }
  }

}
