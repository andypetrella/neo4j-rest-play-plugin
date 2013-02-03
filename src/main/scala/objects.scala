package play.neo4j.rest

import play.api.libs.json.JsObject

case class Root(neo4jVersion:String, node:String)
case class Node(self:String, data:JsObject)
case class Relationship()