package be.nextlab.play.neo4j.rest

import org.specs2.Specification
import play.api.test._
import play.api.test.Helpers._
import Neo4JTestHelpers._
import Neo4JElement._
import play.api.libs.json.{JsNumber, JsString, JsObject}
import scala.util.Random
/**
 *
 * User: noootsab
 * Date: 1/04/12
 * Time: 10:41
 */

object UniqueNodeTest extends Specification {
  val indexName = "uniqueNodeTest"

  def is = "Deal with unique nodes in Neo4J using the REST api " ^ {
      "Create a new UniqueNode " ! neoApp {
        val root = endPoint.root
        val key: String = BigInt(10, Random).toString(36)
        val value: String = BigInt(10, Random).toString(36)
        val node: UniqueNode = UniqueNode(JsObject(Seq("data" -> JsObject(Seq(key -> JsString(value))))))
        await(root flatMap (_.createUniqueNode(node, key -> value)(indexName))) must be like {
          case n: UniqueNode => {
            (n.id must be_>=(0)) and (n.self must not beEmpty) and (n.data must be_==(node.data))
          }
          case x => ko(" is not ok because we didn't got a UniqueNode, but " + x)
        }
      } ^
      "Create a new UniqueNode which already exists " ! neoApp {
        val root = endPoint.root
        val key: String = BigInt(10, Random).toString(36)
        val value: String = BigInt(10, Random).toString(36)
        val node: UniqueNode = UniqueNode(JsObject(Seq("data" -> JsObject(Seq(key -> JsString(value))))))
        await(root flatMap (_.createUniqueNode(node, key -> value)(indexName)))
        await(root flatMap (_.createUniqueNode(node, key -> value)(indexName))) must throwA(new IllegalStateException("Create UniqueNode has been tried but node already exists"))
      } ^
      "Retrieve a unique node using the indexed value " ! {
        val root = endPoint.root
        val key: String = BigInt(10, Random).toString(36)
        val value: String = BigInt(10, Random).toString(36)
        val node: UniqueNode = UniqueNode(JsObject(Seq("data" -> JsObject(Seq(key -> JsString(value))))))

        await(for (
          r <- endPoint.root;
          c <- r.createUniqueNode(node, key -> value)(indexName);
          g <- r.getUniqueNode(key -> value)(indexName)
        ) yield g) must be like {
          case n: UniqueNode => {
            (n.id must be_>=(0)) and (n.self must not beEmpty) and (n.data must be_==(node.data))
          }
          case x => ko(" is not ok because we didn't got a UniqueNode, but " + x)
        }
      } //todo test update and delete...
  }

}
