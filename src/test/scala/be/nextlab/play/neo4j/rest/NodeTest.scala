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

object NodeTest extends Specification {

  def rnds = BigInt(100, scala.util.Random).toString(36)

  def is = "Deal with nodes in Neo4J using the REST api " ^ {
    "Get the reference Node " !
      neoApp {
        val root = endPoint.root
        await(root flatMap (_.referenceNode)) must be like {
          case n: Node => ok(" is ok since we got a Node")
          case x => ko(" is not ok because we didn't got a Node, but " + x)
        }
      } ^
      "Create a new Empty Node " ! neoApp {
        val root = endPoint.root
        await(root flatMap (_.createNode(None))) must be like {
          case n: Node => ok(" is ok since we got a Node")
          case x => ko(" is not ok because we didn't got a Node, but " + x)
        }
      } ^
      "Create a new Node w/ Properties " ! neoApp {
        val root = endPoint.root
        val node: Node = Node(JsObject(Seq("data" -> JsObject(Seq("a" -> JsString("valueForA"))))))
        await(root flatMap (_.createNode(Some(node)))) must be like {
          case n: Node => {
            (n.id must be_>=(0)) and (n.self must not beEmpty) and (n.data must be_==(node.data))
          }
          case x => ko(" is not ok because we didn't got a Node, but " + x)
        }
      } ^
      "Update the data of Node w/ Properties " ! neoApp {
        val root = endPoint.root
        val node: Node = Node(JsObject(Seq("data" -> JsObject(Seq("a" -> JsString("valueForA"))))))
        val newProps: JsObject = JsObject(Seq("a" -> JsString("newValueForA"), "b" -> JsNumber(1)))
        await(for (
          r <- root;
          c <- r.createNode(Some(node));
          u <- c.asInstanceOf[Node].properties(Some(newProps))
        ) yield (c, u)) must be like {
          case (created: Node, n: Node) => {
            (n.id must be_==(created.id)) and (n.self must be_==(created.self)) and (n.data must be_==(newProps))
          }
          case x => ko(" is not ok because we didn't got a Node, but " + x)
        }
      } ^
      "Delete a Node " ! neoApp {
        val root = endPoint.root
        val node: Node = Node(JsObject(Seq("data" -> JsObject(Seq("a" -> JsString("valueForA"))))))
        await(for (
          r <- root;
          c <- r.createNode(Some(node));
          d <- c.asInstanceOf[Node].delete
        ) yield (c, d)) must be like {
          case (c: Node, n: Node) => n must be_==(c) and {
            await(root.flatMap(_.getNode(n.id))) must be like {
              case f: Failure => ok("Get the deleted must return a Failure instance")
              case x => ko("Get the deleted must return a Failure instance, got " + x)
            }
          }
          case x => ko(" is not ok because we didn't got a Node, but " + x)
        }
      } ^
      "Create a Node with one unique index" ! neoApp {
        val root = endPoint.root
        val key: String = rnds
        val value: JsString = JsString(rnds)
        val indexName: String = "uniqueNodeIndex"
        val indexFunction: (JsObject) => JsValue = (js: JsObject) => (js \ "data").as[JsObject] \ key
        val node: Node = Node(JsObject(Seq("data" -> JsObject(Seq(key -> value)))), Seq((indexName, true, "uniqueKey", indexFunction)))
        await(for (
          r <- root;
          c <- r.createNode(Some(node));
          f <- r.getUniqueNode(("uniqueKey", value))(indexName)
        ) yield f) must beSome[Neo4JElement].which {
          found => found must be like {
            case n: Node => indexFunction(n.jsValue) must be_==(value)
            case x => ko(" is not ok because we didn't got a Node, but " + x)
          }
        }
      } ^
      "Update a Node with one unique index" ! neoApp {
        val root = endPoint.root
        val key: String = rnds
        val value: JsString = JsString(rnds)
        val newValue: JsString = JsString(rnds)
        val indexName: String = "uniqueNodeIndex"
        val indexFunction: (JsObject) => JsValue = (js: JsObject) => (js \ "data").as[JsObject] \ key

        val props: JsObject = JsObject(Seq(key -> value))
        val newProps: JsObject = JsObject(Seq(key -> newValue))
        val node: Node = Node(JsObject(Seq("data" -> props)), Seq((indexName, true, "uniqueKey", indexFunction)))

        await(for (
          r <- root;
          c <- r.createNode(Some(node));
          u <- c.asInstanceOf[Node].properties(Some(newProps));
          f <- r.getUniqueNode(("uniqueKey", newValue))(indexName)
        ) yield f) must beSome[Neo4JElement].which {
          n =>
            n must be like {
              case n: Node => indexFunction(n.jsValue) must be_==(newValue)
              case x => ko(" is not ok because we didn't got a Node, but " + x)
            }
        }
      } ^
      "Update a Node with one unique index and get with old value is None" ! neoApp {
        val root = endPoint.root
        val key: String = rnds
        val value: JsString = JsString(rnds)
        val newValue: JsString = JsString(rnds)
        val indexName: String = "uniqueNodeIndex"
        val indexFunction: (JsObject) => JsValue = (js: JsObject) => (js \ "data").as[JsObject] \ key

        val props: JsObject = JsObject(Seq(key -> value))
        val newProps: JsObject = JsObject(Seq(key -> newValue))
        val node: Node = Node(JsObject(Seq("data" -> props)), Seq((indexName, true, "uniqueKey", indexFunction)))

        await(for (
          r <- root;
          c <- r.createNode(Some(node));
          u <- c.asInstanceOf[Node].properties(Some(newProps));
          f <- r.getUniqueNode(("uniqueKey", value))(indexName)
        ) yield f) must beNone
      } ^
      "Delete a Node with one unique index (find it back is None)" ! neoApp {
        val root = endPoint.root
        val key: String = rnds
        val value: JsString = JsString(rnds)
        val indexName: String = "uniqueNodeIndex"
        val indexFunction: (JsObject) => JsValue = (js: JsObject) => (js \ "data").as[JsObject] \ key

        val node: Node = Node(JsObject(Seq("data" -> JsObject(Seq(key -> value)))), Seq((indexName, true, "uniqueKey", indexFunction)))

        await(for (
          r <- root;
          c <- r.createNode(Some(node));
          d <- c.asInstanceOf[Node].delete;
          f <- r.getUniqueNode(("uniqueKey", value))(indexName)
        ) yield f) must beNone
      }
  }

}