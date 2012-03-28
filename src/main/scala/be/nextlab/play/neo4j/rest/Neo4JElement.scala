package be.nextlab.play.neo4j.rest

import scalaz.Monoid
import play.api.libs.json._
import play.api.libs.json.Json._
import dispatch._
import be.nextlab.play.neo4j.rest.util.PlayJsonDispatch._


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
    Http(neo.request(Left(_cypher)) <<(stringify(query), "application/json") <:< Map("Accept" -> "application/json")
      |>! {
      {
        case j: JsObject => CypherResult(j)
        case _ => throw new IllegalStateException("Get Node must return a JsObject")
      }
    }
    )


  //////REFERENCE/////
  lazy val _referenceNode = (jsValue \ "reference_node").as[String]

  def referenceNode(implicit neo: Neo4JEndPoint) = getNode(_referenceNode)

  //////NODE/////
  lazy val _node = (jsValue \ "node").as[String]

  def getNode(id: Int)(implicit neo: Neo4JEndPoint): Neo4JElement = getNode(_node + "/" + id)

  def getNode(url: String)(implicit neo: Neo4JEndPoint): Neo4JElement = Http((neo.request(Left(url)) <:< Map("Accept" -> "application/json") |>! ({
    case j: JsObject => Node(j)
    case _ => throw new IllegalStateException("Get Node must return a JsObject")
  })) >! {
    case e => {
      //todo find the type of the exception thrown for a 404 => node not existing

      //todo remove this...
      e.printStackTrace()
      //todo how to retrieve the error message (json)...
      Failure(
        JsObject(Seq(
          "message" -> JsString(e.getMessage),
          "exception" -> JsString(e.toString),
          "stacktrace" -> JsArray())
        )
      )
    }
  })

  def createNode(n: Option[Node])(implicit neo: Neo4JEndPoint) = {
    val request = n match {
      case None => neo.request(Left(_node)).POST
      case Some(node) => neo.request(Left(_node)) << (stringify(node.data), "application/json")
    }

    Http(request <:< Map("Accept" -> "application/json")
      |>! {
      {
        case j: JsObject => Node(j)
        case _ => throw new IllegalStateException("Get Node must return a JsObject")
      }
    }
    )
  }

}

case class Node(jsValue: JsObject) extends Neo4JElement {
  type Js = JsObject

  //url to it self
  lazy val self = (jsValue \ "self").as[String]
  lazy val id = self.substring(self.lastIndexOf('/')+1).toInt

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
  lazy val properties = (jsValue \ "properties").as[String]

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

  
  def result = data.map { one => {
    columns zip one
  }}
}

case class Failure(jsValue: JsObject) extends Neo4JElement {
  type Js = JsObject


  lazy val message = (jsValue \ "message").as[String]
  lazy val exception = (jsValue \ "exception").as[String]

  lazy val stacktrace = (jsValue \ "stacktrace").as[List[String]]

}


case class Empty() extends Neo4JElement {
  type Js = JsNull.type

  lazy val jsValue = JsNull
}
