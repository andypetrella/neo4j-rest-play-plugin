package be.nextlab.play.neo4j.rest

import play.api.Play.current

import play.api.Logger

import play.api.libs.json._
import play.api.libs.json.Json._

import play.api.libs.concurrent.Akka

import play.api.libs.ws.WS.WSRequestHolder

import be.nextlab.play.neo4j.rest.{Neo4JEndPoint => NEP}
import be.nextlab.play.neo4j.rest.Neo4JEndPoint._


import scalaz.{Failure => KO, Success =>  OK, _ }
import scalaz.Scalaz._

import scala.concurrent.Promise
import scala.concurrent.Future

//Akkaz: implementation of Functor[Future] and Monad[Future]
import scalaz.akkaz.future._
/**
 * User: andy
 */
trait RootElement { self: Neo4JElement =>

  import Neo4JElement._
  import JsValueHelper._

  implicit val executionContext = Akka.system.dispatcher

  type Js = JsObject

  lazy val neo4jVersion = (jsValue \ "neo4j_version").as[String]


  lazy val relationshipTypes = (jsValue \ "relationship_types").as[String]


  lazy val batch = (jsValue \ "batch").as[String]


  lazy val extensionsInfo = (jsValue \ "extensions_info").as[String]

  lazy val extensions = (jsValue \ "extensions").as[JsObject]


  //////REFERENCE/////
  lazy val _referenceNode = (jsValue \ "reference_node").as[String]

  def referenceNode(implicit neo: NEP) = getNode(_referenceNode)


  //////NODE/////
  lazy val _node = (jsValue \ "node").as[String]

  def getNode(id: Int)(implicit neo: NEP): EitherT[Future, Aoutch, Node] = getNode(_node + "/" + id)

  def getNode(url: String)(implicit neo: NEP): EitherT[Future, Aoutch, Node] =
    neo.request(Left(url)) acceptJson() get() map {
      resp =>
        resp.status match {
          case 200 => resp.encJson match {
            case jo: JsObject => Node(jo).right
            case _ => aoutch(new IllegalArgumentException("Get Node must return a JsObject")).left
          }
          case 404 => {
            Logger.error("Error 404 for getNode")
            Logger.debug("Response Body:\n" + resp.body)
            resp.encJson match {
            case jo: JsObject => aoutch(Failure(jo, 404, "Node not Found")).left
            case _ => aoutch(new IllegalStateException("Get Node (404) must return a JsObject")).left
          }}
        }
    }

  def createNode(n: Option[Node])(implicit neo: NEP): EitherT[Future, Aoutch, Node] = {
    val holder: WSRequestHolder = neo.request(Left(_node)) acceptJson()

    for {
      n <- EitherT((n match {
          case None => holder post (JsObject(Seq()))
          case Some(node) => holder post (node.data)
        }) map { resp =>
          resp.status match {
            case 201 => resp.encJson match {
              case jo: JsObject => Node(jo, n map { _.indexes } getOrElse (Nil)).right
              case _ => aoutch(new IllegalArgumentException("Create Node must return a JsObject")).left
            }
            case x => {
              Logger.error("Error "+x+" for createNode")
              Logger.debug("Response Body:\n" + resp.body)
              aoutch(new IllegalStateException("Cannot create Node, error code " + x)).left
            }
          }
        })
      x <- n.applyIndexes//.run map { v => v.fold( //TODO : use recover probably
      //     f => {
      //       n.delete //WARN:: we delete because indexes has failed...
      //       v
      //     },
      //     s => v
      //   )
      // }

    } yield x



  }


  def byUniqueIndex[E<:Entity[E]](index:Index)(implicit neo:NEP, builder:EntityBuilder[E]):JsValue => EitherT[Future, Aoutch, Option[E]] =
    jsValue =>
      neo.request(Left(_nodeIndex + "/" + index.name + "/" + index.key + "/" + jsToString(jsValue))) acceptJson() get() map {
        resp => {
          resp.status match {
            case 200 => resp.encJson match {
              case jo: JsObject => Some(builder(jo, Seq())).right
              case ja: JsArray => ja.value match {
                case Nil => None.right
                case (a: JsObject) :: Nil => Some(builder(a, Seq())).right
                case x => aoutch(new IllegalArgumentException("Get Unique Entity must return a JsObject or a singleton array and not " + x)).left
              }
              case x => aoutch(new IllegalStateException("Get Unique Entity must return a JsObject or a singleton array and not " + x)).left
            }
            case 404 => {
              Logger.error("Error 404 for getUniqueEntity")
              Logger.debug("Response Body:\n" + resp.body)
              resp.encJson match {
                case jo: JsObject => aoutch(Failure(jo, 404, "Unique Entity not Found")).left
                case x => aoutch(new IllegalStateException("Get Unique Entity (errored) must return a JsObject and not " + x)).left
              }}
            case 500 => {
              Logger.error("Error 500 for getUniqueEntity")
              Logger.debug("Response Body:\n" + resp.body)
              resp.encJson match {
                case jo: JsObject => aoutch(Failure(jo, 500, "Unique Entity Crashed")).left
                case x => aoutch(new IllegalStateException("Get Unique Entity (errored) must return a JsObject and not " + x)).left
              }}
            case x => {
              Logger.error("Error "+x+" for getUniqueEntity")
              Logger.debug("Response Body:\n" + resp.body)
              aoutch(new IllegalStateException("Get Unique Entity, error code " + x)).left
            }
          }}
      }

  def getUniqueNode(key: String, value: JsValue)(indexName: String)(implicit neo: NEP): EitherT[Future, Aoutch, Option[Node]] =
    neo.request(Left(_nodeIndex + "/" + indexName + "/" + key + "/" + jsToString(value))) acceptJson() get() map {
      resp => {
        resp.status match {
          case 200 => resp.encJson match {
            case jo: JsObject => Some(Node(jo)).right
            case ja: JsArray => ja.value match {
              case Nil => None.right
              case (a: JsObject) :: Nil => Some(Node(a)).right
              case x => aoutch(new IllegalArgumentException("Get UniqueNode must return a JsObject or a singleton array and not " + x)).left
            }
            case x => aoutch(new IllegalStateException("Get Unique Node must return a JsObject or a singleton array and not " + x)).left
          }
          case 404 => {
            Logger.error("Error 404 for getUniqueNode")
            Logger.debug("Response Body:\n" + resp.body)
            resp.encJson match {
              case jo: JsObject => aoutch(Failure(jo, 404, "Unique Node not Found")).left
              case x => aoutch(new IllegalArgumentException("Get Unique Node (errored) must return a JsObject and not " + x)).left
            }}
          case 500 => {
            Logger.error("Error 500 for getUniqueNode")
            Logger.debug("Response Body:\n" + resp.body)
            resp.encJson match {
              case jo: JsObject => aoutch(Failure(jo, 500, "Unique Node Crashed")).left
              case x => aoutch(new IllegalArgumentException("Get Unique Node (errored) must return a JsObject and not " + x)).left
            }}
          case x => {
            Logger.error("Error "+x+" for getUniqueNode")
            Logger.debug("Response Body:\n" + resp.body)
            aoutch(new IllegalStateException("Get Unique Node, error code " + x)).left
          }
        }}
    }

  //////CYPHER/////
  lazy val _cypher = (jsValue \ "cypher").as[String]

  def cypher(c: Cypher)(implicit neo: NEP):EitherT[Future, Aoutch, CypherResult] =
    neo.request(Left(_cypher)) acceptJson() post (c.toQuery) map {
      resp =>
        resp.status match {
          case 200 => resp.encJson match {
            case j: JsObject => CypherResult(j).right
            case _ => aoutch(new IllegalArgumentException("Get Node must return a JsObject")).left
          }
          case x if x == 400 => resp.encJson match {
            case j: JsObject => aoutch(Failure(j, x, "Fail to execute cypher")).left
            case _ => aoutch(new IllegalArgumentException("Not recognized failure")).left
          }
          case x if x == 500 => resp.encJson match {
            case j: JsObject => aoutch(Failure(j, x, "Fail to execute cypher")).left
            case _ => aoutch(new IllegalArgumentException("Not recognized failure")).left
          }
        }
    }

  //////NODE INDEXES/////
  lazy val _nodeIndex = (jsValue \ "node_index").as[String]
  //////RELATION INDEXES/////
  lazy val _relationshipIndex = (jsValue \ "relationship_index").as[String]


}