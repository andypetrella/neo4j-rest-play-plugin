package be.nextlab.play.neo4j.rest

import scalaz.Monoid
import play.api.libs.json._

/**
 * User: andy
 */
sealed abstract class Neo4JElement {
  type Js <: JsValue

  val jsValue:Js
}

case class Root(jsValue:JsObject) extends Neo4JElement {
  type Js = JsObject

  lazy val neo4jVersion       = (jsValue \ "neo4j_version").as[String]

  lazy val referenceNode      = (jsValue \ "reference_node").as[String]

  lazy val node               = (jsValue \ "node").as[String]
  lazy val relationshipTypes  = (jsValue \ "relationship_types").as[String]

  lazy val nodeIndex          = (jsValue \ "node_index").as[String]
  lazy val relationshipIndex  = (jsValue \ "relationship_index").as[String]

  lazy val batch              = (jsValue \ "batch").as[String]

  lazy val cypher             = (jsValue \ "cypher").as[String]

  lazy val extensionsInfo     = (jsValue \ "extensions_info").as[String]

  lazy val extensions         = (jsValue \ "extensions").as[JsObject]

}

case class Node(jsValue:JsObject) extends Neo4JElement {
  type Js = JsObject

  //url to it self
  lazy val self                         = (jsValue \ "self").as[String]
  //object holding properties
  lazy val data                         = (jsValue \ "data").as[JsObject]

  lazy val traverse                     = (jsValue \ "traverse").as[String]
  lazy val pagedTraverse                = (jsValue \ "paged_traverse").as[String]

  lazy val allRelationships             = (jsValue \ "all_relationships").as[String]
  lazy val allTypedRelationships        = (jsValue \ "all_typed_relationships").as[String]
  lazy val incomingRelationships        = (jsValue \ "incoming_relationships").as[String]
  lazy val incomingTypedRelationships   = (jsValue \ "incoming_typed_relationships").as[String]
  lazy val outgoingRelationships        = (jsValue \ "outgoing_relationships").as[String]
  lazy val outgoingTypedRelationships   = (jsValue \ "outgoing_typed_relationships").as[String]

  lazy val createRelationship           = (jsValue \ "create_relationship").as[String]

  lazy val property                     = (jsValue \ "property").as[String]
  lazy val properties                   = (jsValue \ "properties").as[String]

  lazy val extensions                   = (jsValue \ "extensions").as[JsObject]

  def ++ (el:Node) = Node(jsValue ++ el.jsValue)

  def id = self.substring(self.lastIndexOf('/') + 1).toInt
}

object Node {
  implicit object monoid extends Monoid[Node] {
    def append(s1: Node, s2: => Node) = s1 ++ s2

    val zero = Node(JsObject(Seq("data" -> JsObject(Seq()))))
  }
}

case class Failure(jsValue:JsObject) extends Neo4JElement {
  type Js = JsObject


  lazy val message                      = (jsValue \ "message").as[String]
  lazy val exception                    = (jsValue \ "exception").as[String]

  lazy val stacktrace                   = (jsValue \ "stacktrace").as[List[String]]

}


case class Empty() extends Neo4JElement {
  type Js = JsNull.type

  lazy val jsValue = JsNull
}
