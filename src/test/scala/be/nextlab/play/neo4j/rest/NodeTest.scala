package be.nextlab.play.neo4j.rest

import org.specs2.Specification
import play.api.test._
import play.api.test.Helpers._
import Neo4JTestHelpers._
import Neo4JElement._
import play.api.libs.json.{JsValue, JsNumber, JsString, JsObject}
import play.api.libs.concurrent.Promise
import scala._
import scala.Predef._
import scalaz.{Success => OK, Failure => KO, _}
import scalaz.Scalaz._
import be.nextlab.play.neo4j.rest.ValidationPromised._

/**
 *
 * User: noootsab
 * Date: 1/04/12
 * Time: 10:41
 */

object NodeTest extends Specification {
  val indexName: String = "uniqueNodeIndex"
  val uniqueKey: String = "uniqueKey"
  def indexFunction(key:String) : JsObject => JsValue = (js: JsObject) => (js \ "data").as[JsObject] \ key
  def uniqueNodeIndex(key:String) =  Index(indexName, true, uniqueKey, indexFunction(key))

  def rnds = BigInt(100, scala.util.Random).toString(36)

  def doSmthgWithRoot[N <: Entity[N]](f: Root => Promise[Validation[Error, N]]):Promise[Validation[Error, N]] = 
      for {
        r <- endPoint.root;
        res <- r.fold(
          f => Promise.pure(r),
          s => f(s)
        )
      } yield res.asInstanceOf[Validation[Error, N]]


  def is = "Use The ROOT" ^ {
    "Use the reference Node " ! neoApp {
      await(endPoint.root /~~> (_.referenceNode)) must be like {
        case OK(_) => ok(" is ok")
        case x => ko(" is not ok because we didn't got a Node, but " + x)
      }
    }
  } ^
    end ^
    "Create Nodes " ^ {
    "Create a new Empty Node " ! neoApp {
      await(endPoint.root /~~> (_.createNode(None))) must be like {
        case OK(_) => ok(" is ok")
        case x => ko(" is not ok because we didn't got a Node, but " + x)
      }
    } ^
      "Create a new Node w/ Properties " ! neoApp {
        val node: Node = Node(Nil, "a" -> JsString("valueForA"))
        await(endPoint.root /~~> (_.createNode(Some(node)))) must be like {
          case OK(n@(Node(_, _))) => {
            (n.id must be_>=(0)) and (n.self must not beEmpty) and (n.data must be_==(node.data))
          }
          case x => ko(" is not ok because we didn't got a Node, but " + x)
        }
      }
  } ^ end ^
    "Update Node " ^ {
    "Update the data of Node w/ Properties " ! neoApp {
      val root = endPoint.root
      val node: Node = Node(Nil, "a" -> JsString("valueForA"))
      val newProps: JsObject = JsObject(Seq("a" -> JsString("newValueForA"), "b" -> JsNumber(1)))
      await(
        root /~~>
        (_.createNode(Some(node))) /~~>
        (n => 
          n.properties(Some(newProps)) /~~> (u => Promise.pure(OK((n, u))))
        )
      ) must be like {
        case OK(((created:Node), (n: Node))) => {
          (n.id must be_==(created.id)) and (n.self must be_==(created.self)) and (n.data must be_==(newProps))
        }
        case x => ko(" is not ok because we didn't got a Node, but " + x)
      }
    }
  } ^ end ^
    "Delete Node" ^ {
    "Delete a Node " ! neoApp {
      val root = endPoint.root
      val node: Node = Node(Nil, "a" -> JsString("valueForA"))
      await(
        root /~~>
        (_.createNode(Some(node))) /~~>
        (c => 
          c.delete /~~> (d => Promise.pure(OK(c, d)))
        ) 
      ) must be like {
        case OK(((c: Node), (n: Node))) => n must be_==(c) and {
          await(root /~~> (_.getNode(n.id))) must be like {
            case KO(Right(Failure(_, 404, _))) => ok("Get the deleted must return a Failure instance with 404 status")
            case x => ko("Get the deleted must return a Failure instance, got " + x)
          }
        }
        case x => ko(" is not ok because we didn't got a Node, but " + x)
      }
    }
  } ^ end ^ "Deal with indexes" ^ {
    "CRUD operation on Node with ONE unique index" ^ {
      "Create a Node" ! neoApp {
        val root = endPoint.root

        val key: String = rnds
        val value: JsString = JsString(rnds)
        val index: Index = uniqueNodeIndex(key)
        val node: Node = Node(Seq(index), key -> value)

        await(
          root /~~>
          (r => r.createNode(Some(node)) /~~>
            (_ => r.getUniqueNode(uniqueKey, value)(indexName))
          )
        ) must be like {
          case OK(Some(n)) => index.f(n.jsValue) must be_==(value)
          case x => ko(" is not ok because we didn't got a Node, but " + x)
        }
      } ^
        "Update a Node" ! neoApp {
          val root = endPoint.root
          val key: String = rnds
          val value: JsString = JsString(rnds)
          val newValue: JsString = JsString(rnds)

          val props = Seq(key -> value)
          val newProps = Seq(key -> newValue)
          val index: Index = uniqueNodeIndex(key)
          val node: Node = Node(Seq(index), props:_*)

          await(
            root /~~>
            (r => 
              r.createNode(Some(node)) /~~> 
                (_.properties(Some(JsObject(newProps)))) /~~>
                (_ => r.getUniqueNode(uniqueKey, newValue)(indexName))
            )
          ) must be like {
            case OK(Some(n)) => index.f(n.jsValue) must be_==(newValue)
            case x => ko(" is not ok because we didn't got a Node, but " + x)
          } 
        } ^
        "Update a Node and get with old value is None" ! neoApp {
          val root = endPoint.root
          val key: String = rnds
          val value: JsString = JsString(rnds)
          val newValue: JsString = JsString(rnds)

          val props = Seq(key -> value)
          val newProps = Seq(key -> newValue)
          val index: Index = uniqueNodeIndex(key)
          val node: Node = Node(Seq(index), props:_*)

          await(
            root /~~>
            (r => 
             r.createNode(Some(node)) /~~> 
              (_.properties(Some(JsObject(newProps)))) /~~>
              (_ => r.getUniqueNode(uniqueKey, value)(indexName))
            )
          ) must be like {
            case OK(None) => ok("Got nothing")
            case x => ko("not ok because we got something " + x)
          }
        } ^
        "Update a Node which have props already updated " ! neoApp {
          val root = endPoint.root
          val key: String = rnds
          val value: JsString = JsString(rnds)
          val newValue: JsString = JsString(rnds)

          val props = Seq(key -> value)
          val newProps = Seq(key -> newValue)
          val index: Index = uniqueNodeIndex(key)
          val node: Node = Node(Seq(index), props:_*)

          //inject the given js object into the node
          def inject(node: Node, js: JsObject) = Node(
            node.jsValue ++ js, // ++ takes only the value from the second
            node.indexes
          )

          await(
            root /~~>
            (r => 
              r.createNode(Some(node)) /~~> 
                (_.updateData(newProps:_*).properties(Some(JsObject(newProps)))) /~~>
                (_ => r.getUniqueNode(uniqueKey, newValue)(indexName))
            )
          ) must like {
            case OK(Some(n)) => index.f(n.jsValue) must be_==(newValue)
            case x => ko(" is not ok because we didn't got a Node, but " + x)
          }
        } ^
        "Update a Node which have props already updated, get w/ old value " ! neoApp {
          val root = endPoint.root
          val key: String = rnds
          val value: JsString = JsString(rnds)
          val newValue: JsString = JsString(rnds)

          val props = Seq(key -> value)
          val newProps = Seq(key -> newValue)
          val index: Index = uniqueNodeIndex(key)
          val node: Node = Node(Seq(index), props:_*)

          //inject the given js object into the node
          def inject(node: Node, js: JsObject) = Node(
            node.jsValue ++ js, // ++ takes only the value from the second
            node.indexes
          )

          await(
            root /~~>
            (r => 
              r.createNode(Some(node)) /~~>
              (_.updateData(newProps:_*).properties(Some(JsObject(newProps)))) /~~>
              (_ => r.getUniqueNode(uniqueKey, value)(indexName))
            )
          ) must be like {
            case OK(None) => ok(" we got nothing")
            case x => ko("is failure because we got something " + x)
          }
        } ^
        "Delete a Node (find it back is None)" ! neoApp {
          val root = endPoint.root

          val key: String = rnds
          val value: JsString = JsString(rnds)
          val index: Index = uniqueNodeIndex(key)
          val node: Node = Node(Seq(index), key -> value)

          await(
            root /~~>
            (r => 
              r.createNode(Some(node)) /~~>
              (_.delete) /~~>
              (_ => r.getUniqueNode(uniqueKey, value)(indexName))
            )
          ) must be like {
            case OK(None) => ok("we got nothing")
            case x => ko("because we got something " + x)
          }
        }
    }
  }

}
