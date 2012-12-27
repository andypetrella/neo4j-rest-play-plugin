package be.nextlab.play.neo4j.rest

import play.api.Logger

import play.api.libs.json._
import play.api.libs.json.Json._

import play.api.libs.concurrent.Promise

import play.api.libs.ws.WS.WSRequestHolder

import be.nextlab.play.neo4j.rest.{Neo4JEndPoint => NEP}
import be.nextlab.play.neo4j.rest.Neo4JEndPoint._


import scalaz.{Failure => KO, Success => OK, Logger =>ZLogger, _}
import scalaz.Scalaz._

import ValidationPromised._

/**
 * User: andy
 */
trait RootElement { self: Neo4JElement =>

  import Neo4JElement._
  import ValidationPromised._
  import JsValueHelper._

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

  def getNode(id: Int)(implicit neo: NEP): ValidationPromised[Aoutch, Node] = getNode(_node + "/" + id)

  def getNode(url: String)(implicit neo: NEP): ValidationPromised[Aoutch, Node] =
    neo.request(Left(url)) acceptJson() get() map {
      resp =>
        resp.status match {
          case 200 => resp.encJson match {
            case jo: JsObject => OK(Node(jo))
            case _ => KO(NonEmptyList[Exception](new IllegalArgumentException("Get Node must return a JsObject")))
          }
          case 404 => {
            Logger.error("Error 404 for getNode")
            Logger.debug("Response Body:\n" + resp.body)
            resp.encJson match {
            case jo: JsObject => KO(NonEmptyList[Exception](Failure(jo, 404, "Node not Found")))
            case _ => KO(NonEmptyList[Exception](new IllegalStateException("Get Node (404) must return a JsObject")))
          }}
        }
    } transformer

  def createNode(n: Option[Node])(implicit neo: NEP): ValidationPromised[Aoutch, Node] = {
    val holder: WSRequestHolder = neo.request(Left(_node)) acceptJson()

    for {
      n <- ((n match {
          case None => holder post (JsObject(Seq()))
          case Some(node) => holder post (node.data)
        }) map { resp =>
          resp.status match {
            case 201 => resp.encJson match {
              case jo: JsObject => OK(Node(jo, n map { _.indexes } getOrElse (Nil)))
              case _ => KO(NonEmptyList[Exception](new IllegalArgumentException("Create Node must return a JsObject")))
            }
            case x => {
              Logger.error("Error "+x+" for createNode")
              Logger.debug("Response Body:\n" + resp.body)
              KO(NonEmptyList[Exception](new IllegalStateException("Cannot create Node, error code " + x)))
            }
          }
        }) transformer;
      x <- n.applyIndexes /~> { v => Promise.pure(v.fold(
          f => {
            n.delete //WARN:: we delete because indexes has failed...
            v
          },
          s => v
        )) transformer
      }
    } yield n
  }


  def byUniqueIndex[E<:Entity[E]](index:Index)(implicit neo:NEP, builder:EntityBuilder[E]):JsValue => ValidationPromised[Aoutch, Option[E]] =
    jsValue =>
      neo.request(Left(_nodeIndex + "/" + index.name + "/" + index.key + "/" + jsToString(jsValue))) acceptJson() get() map {
        resp => {
          resp.status match {
            case 200 => resp.encJson match {
              case jo: JsObject => OK(Some(builder(jo, Seq())))
              case ja: JsArray => ja.value match {
                case Nil => OK(None)
                case (a: JsObject) :: Nil => OK(Some(builder(a, Seq())))
                case x => KO(NonEmptyList(new IllegalArgumentException("Get Unique Entity must return a JsObject or a singleton array and not " + x)))
              }
              case x => KO(NonEmptyList(new IllegalStateException("Get Unique Entity must return a JsObject or a singleton array and not " + x)))
            }
            case 404 => {
              Logger.error("Error 404 for getUniqueEntity")
              Logger.debug("Response Body:\n" + resp.body)
              resp.encJson match {
                case jo: JsObject => KO(NonEmptyList(Failure(jo, 404, "Unique Entity not Found")))
                case x => KO(NonEmptyList(new IllegalStateException("Get Unique Entity (errored) must return a JsObject and not " + x)))
              }}
            case 500 => {
              Logger.error("Error 500 for getUniqueEntity")
              Logger.debug("Response Body:\n" + resp.body)
              resp.encJson match {
                case jo: JsObject => KO(NonEmptyList(Failure(jo, 500, "Unique Entity Crashed")))
                case x => KO(NonEmptyList(new IllegalStateException("Get Unique Entity (errored) must return a JsObject and not " + x)))
              }}
            case x => {
              Logger.error("Error "+x+" for getUniqueEntity")
              Logger.debug("Response Body:\n" + resp.body)
              KO(NonEmptyList(new IllegalStateException("Get Unique Entity, error code " + x)))
            }
          }}
      } transformer

  def getUniqueNode(key: String, value: JsValue)(indexName: String)(implicit neo: NEP): ValidationPromised[Aoutch, Option[Node]] =
    neo.request(Left(_nodeIndex + "/" + indexName + "/" + key + "/" + jsToString(value))) acceptJson() get() map {
      resp => {
        resp.status match {
          case 200 => resp.encJson match {
            case jo: JsObject => OK(Some(Node(jo)))
            case ja: JsArray => ja.value match {
              case Nil => OK(None)
              case (a: JsObject) :: Nil => OK(Some(Node(a)))
              case x => KO(NonEmptyList(new IllegalArgumentException("Get UniqueNode must return a JsObject or a singleton array and not " + x)))
            }
            case x => KO(NonEmptyList(new IllegalStateException("Get Unique Node must return a JsObject or a singleton array and not " + x)))
          }
          case 404 => {
            Logger.error("Error 404 for getUniqueNode")
            Logger.debug("Response Body:\n" + resp.body)
            resp.encJson match {
              case jo: JsObject => KO(NonEmptyList(Failure(jo, 404, "Unique Node not Found")))
              case x => KO(NonEmptyList(new IllegalArgumentException("Get Unique Node (errored) must return a JsObject and not " + x)))
            }}
          case 500 => {
            Logger.error("Error 500 for getUniqueNode")
            Logger.debug("Response Body:\n" + resp.body)
            resp.encJson match {
              case jo: JsObject => KO(NonEmptyList(Failure(jo, 500, "Unique Node Crashed")))
              case x => KO(NonEmptyList(new IllegalArgumentException("Get Unique Node (errored) must return a JsObject and not " + x)))
            }}
          case x => {
            Logger.error("Error "+x+" for getUniqueNode")
            Logger.debug("Response Body:\n" + resp.body)
            KO(NonEmptyList(new IllegalStateException("Get Unique Node, error code " + x)))
          }
        }}
    } transformer

  //////CYPHER/////
  lazy val _cypher = (jsValue \ "cypher").as[String]

  def cypher(c: Cypher)(implicit neo: NEP):ValidationPromised[Aoutch, CypherResult] =
    neo.request(Left(_cypher)) acceptJson() post (c.toQuery) map {
      resp =>
        resp.status match {
          case 200 => resp.encJson match {
            case j: JsObject => OK(CypherResult(j))
            case _ => KO(NonEmptyList(new IllegalArgumentException("Get Node must return a JsObject")))
          }
          case x if x == 400 => resp.encJson match {
            case j: JsObject => KO(NonEmptyList(Failure(j, x, "Fail to execute cypher")))
            case _ => KO(NonEmptyList(new IllegalArgumentException("Not recognized failure")))
          }
          case x if x == 500 => resp.encJson match {
            case j: JsObject => KO(NonEmptyList(Failure(j, x, "Fail to execute cypher")))
            case _ => KO(NonEmptyList(new IllegalArgumentException("Not recognized failure")))
          }
        }
    } transformer

  //////NODE INDEXES/////
  lazy val _nodeIndex = (jsValue \ "node_index").as[String]
  //////RELATION INDEXES/////
  lazy val _relationshipIndex = (jsValue \ "relationship_index").as[String]


}