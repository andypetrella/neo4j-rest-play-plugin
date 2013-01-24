package be.nextlab.play.neo4j.rest

import play.api.Play.current

import play.api.libs.json._
import play.api.libs.json.Json._

import play.api.libs.concurrent.Akka

import play.api.libs.ws.Response
import play.api.libs.ws.WS.WSRequestHolder

import be.nextlab.play.neo4j.rest.{Neo4JEndPoint => NEP}
import be.nextlab.play.neo4j.rest.Neo4JEndPoint._


import scalaz.Monoid
// import scalaz.Scalaz._

import scala.concurrent.{Future, ExecutionContext}

//Akkaz: implementation of Functor[Future] and Monad[Future]
// import scalaz.akkaz.future._

/**
 * User: andy
 */
trait RootElement { self: Neo4JElement =>

  import Neo4JElement._
  import JsValueHelper._

  //implicit val executionContext = Akka.system.dispatcher

  type Js = JsObject

  lazy val neo4jVersion = (jsValue \ "neo4j_version").as[String]


  lazy val relationshipTypes = (jsValue \ "relationship_types").as[String]


  lazy val batch = (jsValue \ "batch").as[String]


  lazy val extensionsInfo = (jsValue \ "extensions_info").as[String]

  lazy val extensions = (jsValue \ "extensions").as[JsObject]


  //////REFERENCE/////
  lazy val _referenceNode = (jsValue \ "reference_node").as[String]

  def referenceNode(implicit neo: NEP, ec:ExecutionContext) = getNode(_referenceNode)


  //////NODE/////
  lazy val _node = (jsValue \ "node").as[String]

  private[this] val findNodeRegex = "(.*/)node$".r
  lazy val _relation =
    _node match {
      case findNodeRegex(pref) => pref+"relationship"
      case _ => throw new IllegalArgumentException(_node + " doens't match " + findNodeRegex)
    }


  def getRelation(id: Int)(implicit neo: NEP, ec:ExecutionContext): Future[Relation] = getRelation(_relation + "/" + id)
  def getRelation(url: String)(implicit neo: NEP, ec:ExecutionContext): Future[Relation] = {
    import RelationElement._
    getEntity(url)
  }

  def getNode(id: Int)(implicit neo: NEP, ec:ExecutionContext): Future[Node] = getNode(_node + "/" + id)
  def getNode(url: String)(implicit neo: NEP, ec:ExecutionContext): Future[Node] = {
    import NodeElement._
    getEntity(url)
  }

  def getEntity[E<:Entity[E]](url: String)(implicit neo: NEP, b:EntityBuilder[E], ec:ExecutionContext): Future[E] =
    neo.request(Left(url)) acceptJson() get() map {
      withNotHandledStatus(Seq(200)) {
        case jo:JsObject => b(jo, Seq.empty)
      }
    }


  def createNode(n: Option[Node])(implicit neo: NEP, ec:ExecutionContext): Future[Node] = {
    val holder: WSRequestHolder = neo.request(Left(_node)) acceptJson()

    for {
      n <- (n match {
                case None => holder post (JsObject(Seq()))
                case Some(node) => holder post (node.data)
            }).map {
              withNotHandledStatus(Seq(201)) {
                case jo: JsObject => Node(jo, n map { _.indexes } getOrElse (Nil))
              }
            }
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

  def createReqaltion(r: Relation)(implicit neo: NEP, ec:ExecutionContext): Future[Relation] = {
    val holder: WSRequestHolder = neo.request(Left(_node)) acceptJson()

    for {
      start <-  r.start;
      rel   <-  start.createRelationship(r)
    } yield rel
  }


  def byUniqueIndex[E<:Entity[E]](index:Index)(implicit neo:NEP, builder:EntityBuilder[E], ec:ExecutionContext):JsValue => Future[Option[E]] =
    jsValue =>
      neo.request(Left(_nodeIndex + "/" + index.name + "/" + index.key + "/" + jsToString(jsValue))) acceptJson() get() map {
        withNotHandledStatus(Seq(200)) {
          case jo: JsObject => Some(builder(jo, Seq()))
          case ja: JsArray => ja.value match {
            case Nil => None
            case (a: JsObject) :: Nil => Some(builder(a, Seq()))
            case x => throw new IllegalArgumentException("Get Unique Entity must return a JsObject or a singleton array and not " + x)
          }
        }
      }

  def getUniqueNode(key: String, value: JsValue)(indexName: String)(implicit neo: NEP, builder:EntityBuilder[Node], ec:ExecutionContext): Future[Option[Node]] = {
    val a = byUniqueIndex(Index(indexName, true, key, (_:JsObject) => value))
    a(value)
  }


  //////CYPHER/////
  lazy val _cypher = (jsValue \ "cypher").as[String]

  def cypher(c: Cypher)(implicit neo: NEP, ec:ExecutionContext):Future[CypherResult] =
    neo.request(Left(_cypher)) acceptJson() post (c.toQuery) map {
      withNotHandledStatus(Seq(200)) {
        case j: JsObject => CypherResult(j)
      }
    }

  //////NODE INDEXES/////
  lazy val _nodeIndex = (jsValue \ "node_index").as[String]
  //////RELATION INDEXES/////
  lazy val _relationshipIndex = (jsValue \ "relationship_index").as[String]

}