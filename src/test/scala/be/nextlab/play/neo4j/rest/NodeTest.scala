package be.nextlab.play.neo4j.rest

import org.specs2.Specification

import play.api.Play.current

import play.api.test._
import play.api.test.Helpers._

import Neo4JTestHelpers._
import Neo4JElement._
import Node._

import play.api.libs.json.{JsValue, JsNumber, JsString, JsObject}

import play.api.libs.concurrent.Akka
import scala.concurrent.Future

/**
 *
 * User: noootsab
 * Date: 1/04/12
 * Time: 10:41
 */

object NodeTest extends Specification {

  implicit lazy val executionContext = Akka.system.dispatcher

  val indexName: String = "uniqueNodeIndex"
  val uniqueKey: String = "uniqueKey"
  def indexFunction(key:String) : JsObject => JsValue = (js: JsObject) => (js \ "data").as[JsObject] \ key
  def uniqueNodeIndex(key:String) =  Index(indexName, true, uniqueKey, indexFunction(key))

  def rnds = BigInt(100, scala.util.Random).toString(36)

  def partialIdentity[A]:PartialFunction[A,A] = {
    case x => x
  }

  def is =
    "Use The ROOT" ^ {
      "Use the reference Node " ! neoApp {
        await(for {
            r <- endPoint.root
            ref <- r.referenceNode
          } yield ref
        ) must be like {
          case x:Node => ok(" is ok")
          case x => ko(" is not ok because we didn't got a Node, but " + x)
        }
      }
    } ^ end ^
    "Create Nodes " ^ {
      "Create a new Empty Node " ! neoApp {
        await(for {
            r <- endPoint.root
            n <- r.createNode(None)
          } yield n
        ) must be like {
          case x:Node => ok(" is ok")
          case x => ko(" is not ok because we didn't got a Node, but " + x)
        }
      } ^
        "Create a new Node w/ Properties " ! neoApp {
          val node: Node = Node(Nil, "a" -> JsString("valueForA"))
          await(for {
              r <- endPoint.root;
              n <- r.createNode(Some(node))
            } yield n
          ) must be like {
            case n@Node(_, _) => {
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
        await(for {
            r <- root;
            n <- r.createNode(Some(node));
            u <- n.properties(Some(newProps))
          } yield (n, u)
        ) must be like {
          case ((created:Node), (n: Node)) => {
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
        await(for {
            r <- root
            c <- r.createNode(Some(node))
            d <- c.delete
          } yield (c, d)
        ) must be like {
          case ((c: Node), (n: Node)) => n must be_==(c) and {
            await(root flatMap (_.getNode(n.id)) recover { partialIdentity[Throwable] }) must be like {
              case bad:BadStatus if bad.status == 404 => ok("Get the deleted must return a Failure instance with 404 status")
              case x => ko("Get the deleted must return a Failure instance, got " + x)
            }
          }
          case x => ko(" is not ok because we didn't got a Node, but " + x)
        }
      }
    } ^ end ^
    "Using links " ^ {
      def prep = for {
        r <- endPoint.root
        c <- r.createNode(Some(Node(Nil, "a"->JsString("a"))))
        _ <- Future.sequence(Seq("A", "B", "D").map{ t =>
              for {
                n <- r.createNode(Some(Node(Nil, "in"->JsString(t))))
                l <- n.createRelationship(Relation(
                  Right(n),
                  Right(c),
                  t,
                  Nil)
                )
              } yield l
            })
        _ <- Future.sequence(Seq("A", "C", "D").map{ t =>
              for {
                n <- r.createNode(Some(Node(Nil, "out"->JsString(t))))
                l <- c.createRelationship(Relation(
                  Right(c),
                  Right(n),
                  t,
                  Nil)
                )
              } yield l
            })
        ia <- c.linkedBy("A", Relation.In)
        oc <- c.linkedBy("C", Relation.Out)
        bd <- c.linkedBy("D", Relation.Both)
      } yield (ia, oc, bd)
      "Get inbounds by type" ^ {

        "retrieve incoming relations" ! neoApp {
          await(prep) must be like {
            case (i, _, _) =>
              i.size must be_==(1) and
              ((i(0) \ "data" \ "in").as[String] must be_==("A"))
          }
        }
      } ^
      "Get outbounds by type" ^ {
        "retrieve outgoing relations" ! neoApp {
          await(prep) must be like {
            case (_, o, _) =>
              o.size must be_==(1) and
              ((o(0) \ "data" \ "out").as[String] must be_==("C"))
          }
        }
      } ^
      "Get linked by type" ^ {
        "retrieve both directions relations" ! neoApp {
          await(prep) must be like {
            case (_, _, b) =>
              b.size must be_==(2) and
              ((b(0) \ "data" \ "in").asOpt[String].orElse((b(0) \ "data" \ "out").asOpt[String]) must be_==(Some("D"))) and
              ((b(1) \ "data" \ "in").asOpt[String].orElse((b(1) \ "data" \ "out").asOpt[String]) must be_==(Some("D")))
          }
        }
      }
    } ^ end ^
    "Deal with indexes" ^ {
      "CRUD operation on Node with ONE unique index" ^ {
        "Create a Node" ! neoApp {
          val root = endPoint.root

          val key:   String   = rnds
          val value: JsString = JsString(rnds)
          val index: Index    = uniqueNodeIndex(key)
          val node:  Node     = Node(Seq(index), key -> value)

          await(for {
              r <- root;
              n <- r.createNode(Some(node));
              u <- r.getUniqueNode(uniqueKey, value)(indexName)
              } yield u
          ) must be like {
            case Some(n) => index.f(n.jsValue) must be_==(value)
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

          await(for {
              r <- root;
              n <- r.createNode(Some(node));
              p <- n.properties(Some(JsObject(newProps)));
              u <- r.getUniqueNode(uniqueKey, newValue)(indexName)
            } yield u
          ) must be like {
            case Some(n) => index.f(n.jsValue) must be_==(newValue)
            case x => ko(" is not ok because we didn't got a Node, but " + x)
          }
        } ^
        "Update one Node property" ! neoApp {
          val root = endPoint.root
          val key: String = rnds
          val value: JsString = JsString(rnds)
          val newValue: JsString = JsString(rnds)

          val props = Seq(key -> value)
          val index: Index = uniqueNodeIndex(key)
          val node: Node = Node(Seq(index), props:_*)

          await(for {
              r <- root;
              n <- r.createNode(Some(node));
              p <- n<+(key, Some(newValue));
              u <- r.getUniqueNode(uniqueKey, newValue)(indexName)
            } yield u
          ) must be like {
            case Some(n) => index.f(n.jsValue) must be_==(newValue)
            case x => ko(" is not ok because we didn't got a Node, but " + x)
          }
        } ^
        "Update one Node property keeps others" ! neoApp {
          val root = endPoint.root
          val keptKeyValue = (rnds, JsString(rnds))
          val key: String = rnds
          val value: JsString = JsString(rnds)
          val newValue: JsString = JsString(rnds)

          val props = Seq(keptKeyValue, key -> value)
          val index: Index = uniqueNodeIndex(key)
          val node: Node = Node(Seq(index), props:_*)

          await(for {
              r <- root;
              n <- r.createNode(Some(node));
              p <- n<+(key, Some(newValue))
              //u <- r.getUniqueNode(uniqueKey, newValue)(indexName)
            } yield p
          ) must be like {
            case n:Node => (n.data \ keptKeyValue._1) must be_==(keptKeyValue._2)
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

          await(for{
            r <- root;
            n <-r.createNode(Some(node));
            p <-n.properties(Some(JsObject(newProps)));
            u <- r.getUniqueNode(uniqueKey, value)(indexName)
            } yield u
          ) must be like {
            case None => ok("Got nothing")
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

          await(for {
            r <- root;
            n <- r.createNode(Some(node));
            p <- n.updateData(newProps:_*).properties(Some(JsObject(newProps)));
            u <- r.getUniqueNode(uniqueKey, newValue)(indexName)
            } yield u
          ) must like {
            case Some(n) => index.f(n.jsValue) must be_==(newValue)
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

          await(for {
            r <- root;
            n <- r.createNode(Some(node));
            p <- n.updateData(newProps:_*).properties(Some(JsObject(newProps)));
            u <- r.getUniqueNode(uniqueKey, value)(indexName)
            } yield u
          ) must be like {
            case None => ok(" we got nothing")
            case x => ko("is failure because we got something " + x)
          }
        } ^
        "Delete a Node (find it back is None)" ! neoApp {
          val root = endPoint.root

          val key: String = rnds
          val value: JsString = JsString(rnds)
          val index: Index = uniqueNodeIndex(key)
          val node: Node = Node(Seq(index), key -> value)

          await(for {
            r <- root;
            n <- r.createNode(Some(node));
            d <- n.delete;
            u <- r.getUniqueNode(uniqueKey, value)(indexName)
            } yield u
          ) must be like {
            case None => ok("we got nothing")
            case x => ko("because we got something " + x)
          }
        }
      }
    }

}
