package be.nextlab.play.neo4j.rest

import play.api.libs.json._

/**
 * User: andy
 */
abstract class Neo4JElement[Js <: JsValue] {
  val jsValue:Js
}

case class Root(jsValue:JsObject) extends Neo4JElement[JsObject] {

  lazy val neo4jVersion       = jsValue \ "neo4j_version"

  lazy val referenceNode      = jsValue \ "reference_node"

  lazy val node               = jsValue \ "node"
  lazy val relationshipTypes  = jsValue \ "relationship_types"

  lazy val nodeIndex          = jsValue \ "node_index"
  lazy val relationshipIndex  = jsValue \ "relationship_index"

  lazy val batch              = jsValue \ "batch"

  lazy val cypher             = jsValue \ "cypher"

  lazy val extensionsInfo     = jsValue \ "extensions_info"

  lazy val extensions         = jsValue \ "extensions"

}

case class Node(jsValue:JsObject) extends Neo4JElement[JsObject] {

  //url to it self
  lazy val self                         = jsValue \ "self"
  //object holding properties
  lazy val data                         = jsValue \ "data" //

  lazy val traverse                     = jsValue \ "traverse"
  lazy val pagedTraverse                = jsValue \ "paged_traverse"

  lazy val allRelationships             = jsValue \ "all_relationships"
  lazy val allTypedRelationships        = jsValue \ "all_typed_relationships"
  lazy val incomingRelationships        = jsValue \ "incoming_relationships"
  lazy val incomingTypedRelationships   = jsValue \ "incoming_typed_relationships"
  lazy val outgoingRelationships        = jsValue \ "outgoing_relationships"
  lazy val outgoingTypedRelationships   = jsValue \ "outgoing_typed_relationships"

  lazy val createRelationship           = jsValue \ "create_relationship"

  lazy val property                     = jsValue \ "property"
  lazy val properties                   = jsValue \ "properties"

  lazy val extensions                   = jsValue \ "extensions"

  def ++ (el:Node) = Node(jsValue ++ el.jsValue)

}

case class Failure(jsValue:JsObject) extends Neo4JElement[JsObject] {

  lazy val message                      = jsValue \ "message"
  lazy val exception                    = jsValue \ "exception"

  lazy val stacktrace                   = jsValue \ "stacktrace" //String list

}


case class Empty() extends Neo4JElement[JsValue] {
  lazy val jsValue = JsNull

}
