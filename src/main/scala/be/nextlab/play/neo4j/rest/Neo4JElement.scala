package be.nextlab.play.neo4j.rest

import scalaz.Monoid
import play.api.libs.json._
import play.api.libs.json.Json._
import Neo4JEndPoint._
import play.api.libs.concurrent.Promise
import play.api.libs.ws.WS.WSRequestHolder


/**
 * User: andy
 */
sealed abstract class Neo4JElement {
  val jsValue: JsValue
}

case class Root(jsValue: JsObject) extends Neo4JElement {

  lazy val neo4jVersion = (jsValue \ "neo4j_version").as[String]


  lazy val relationshipTypes = (jsValue \ "relationship_types").as[String]

  lazy val nodeIndex = (jsValue \ "node_index").as[String]
  lazy val relationshipIndex = (jsValue \ "relationship_index").as[String]

  lazy val batch = (jsValue \ "batch").as[String]


  lazy val extensionsInfo = (jsValue \ "extensions_info").as[String]

  lazy val extensions = (jsValue \ "extensions").as[JsObject]

  //////CYPHER/////
  lazy val _cypher = (jsValue \ "cypher").as[String]

  def cypher(query: JsObject)(implicit neo: Neo4JEndPoint) =
    neo.request(Left(_cypher)) acceptJson() post (query) map {
      resp =>
        resp.status match {
          case 200 => resp.json match {
            case j: JsObject => CypherResult(j)
            case _ => throw new IllegalStateException("Get Node must return a JsObject")
          }
        }
    }


  //////REFERENCE/////
  lazy val _referenceNode = (jsValue \ "reference_node").as[String]

  def referenceNode(implicit neo: Neo4JEndPoint) = getNode(_referenceNode)

  //////NODE/////
  lazy val _node = (jsValue \ "node").as[String]

  def getNode(id: Int)(implicit neo: Neo4JEndPoint): Promise[Neo4JElement] = getNode(_node + "/" + id)

  def getNode(url: String)(implicit neo: Neo4JEndPoint): Promise[Neo4JElement] = neo.request(Left(url)) acceptJson() get() map {
    resp =>
      resp.status match {
        case 200 => resp.json match {
          case jo: JsObject => Node(jo)
          case _ => throw new IllegalStateException("Get Node must return a JsObject")
        }
        case 404 => resp.json match {
          case jo: JsObject => Failure(jo, 404, "Node not Found")
          case _ => throw new IllegalStateException("Get Node (errored) must return a JsObject")
        }
      }
  }

  def createNode(n: Option[Node])(implicit neo: Neo4JEndPoint) = {
    val holder: WSRequestHolder = neo.request(Left(_node)) acceptJson()

    (n match {
      case None => holder post(JsObject(Seq()))
      case Some(node) => holder post (node.data)
    }) map {
      resp =>
        resp.status match {
          case 201 => resp.json match {
            case jo: JsObject => Node(jo)
            case _ => throw new IllegalStateException("Create Node must return a JsObject")
          }
        }
    }
  }

}

case class Node(jsValue: JsObject) extends Neo4JElement {
  type Js = JsObject

  //url to it self
  lazy val self = (jsValue \ "self").as[String]
  lazy val id = self.substring(self.lastIndexOf('/') + 1).toInt

  //object holding properties
  lazy val data = (jsValue \ "data").as[JsObject]

  lazy val traverse = (jsValue \ "traverse").as[String]
  lazy val pagedTraverse = (jsValue \ "paged_traverse").as[String]

  lazy val allRelationships = (jsValue \ "all_relationships").as[String]
  lazy val allTypedRelationships = (jsValue \ "all_typed_relationships").as[String]
  lazy val incomingRelationships = (jsValue \ "incoming_relationships").as[String]
  lazy val incomingTypedRelationships = (jsValue \ "incoming_typed_relationships").as[String]
  lazy val outgoingRelationships = (jsValue \ "outgoing_relationships").as[String]
  lazy val outgoingTypedRelationships = (jsValue \ "outgoing_typed_relationships").as[String]

  lazy val createRelationship = (jsValue \ "create_relationship").as[String]

  lazy val property = (jsValue \ "property").as[String]

  lazy val _properties = (jsValue \ "properties").as[String]

  def properties(data: Option[JsObject])(implicit neo: Neo4JEndPoint): Promise[Neo4JElement] = data match {

    case Some(d) => neo.request(Left(_properties)) acceptJson() put (d) map { resp =>
      resp.status match {
        case 204 => this ++ Node(JsObject(Seq("data" -> d)))
        case x => throw new IllegalStateException("TODO : update props error " + x)
      }
    }

    case None => neo.request(Left(_properties)) acceptJson() get() map {
      resp =>
        resp.status match {
          case 200 => resp.json match {
            case j: JsObject => this ++ Node(JsObject(Seq("data" -> j)))
            case _ => throw new IllegalStateException("Get Properties Node must return a JsObject")
          }
          case x => throw new IllegalStateException("TODO : get props error " + x)
        }
    }
  }

  def delete(implicit neo: Neo4JEndPoint): Promise[Neo4JElement] =
    neo.request(Left(self)) acceptJson() delete() map { resp =>
      resp.status match {
          case 204 => this
          case 409 => resp.json match {
            case jo:JsObject => Failure(jo, 409, "Cannot Delete Node with relations")
            case _ => throw new IllegalStateException("delete Node must return a JsObject")
          }
          case x => throw new IllegalStateException("TODO : delete node error " + x)
        }
    }


  lazy val extensions = (jsValue \ "extensions").as[JsObject]

  def ++(el: Node) = Node(jsValue ++ el.jsValue)

}

object Node {

  implicit object monoid extends Monoid[Node] {
    def append(s1: Node, s2: => Node) = s1 ++ s2

    val zero = Node(JsObject(Seq("data" -> JsObject(Seq()))))
  }

}

case class CypherResult(jsValue: JsObject) extends Neo4JElement {
  type Js = JsObject

  lazy val data = (jsValue \ "data").as[Seq[Seq[JsValue]]]
  lazy val columns = (jsValue \ "columns").as[Seq[String]]


  def result = data.map {
    one => {
      columns zip one
    }
  }
}

case class Failure(jsValue: JsObject, status:Int, info:String) extends Neo4JElement {
  type Js = JsObject


  lazy val message = (jsValue \ "message").as[String]
  lazy val exception = (jsValue \ "exception").as[String]

  lazy val stacktrace = (jsValue \ "stacktrace").as[List[String]]

}


case class Empty() extends Neo4JElement {
  type Js = JsNull.type

  lazy val jsValue = JsNull
}
