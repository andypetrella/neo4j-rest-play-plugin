package be.nextlab.play.neo4j.rest

import play.api.Logger

import play.api.libs.json._
import play.api.libs.json.Json._

import play.api.libs.concurrent.Promise

import play.api.libs.ws.WS.WSRequestHolder

import be.nextlab.play.neo4j.rest.{Neo4JEndPoint => NEP}
import be.nextlab.play.neo4j.rest.Neo4JEndPoint._


import scalaz.{Failure => KO, Success => OK, _}
import scalaz.Scalaz._

import scala.concurrent.Future
/**
 * User: andy
 */
sealed trait Neo4JElement {
  type Js <: JsValue

  val jsValue: Js
}

case class Root(jsValue: JsObject) extends RootElement with Neo4JElement {
}

case class Cypher(query:String, params:(String, JsValue)*) {
  def toQuery = JsObject(Seq(
    "query" -> JsString(query),
    "params" -> JsObject(params.toSeq)
  ))
}
case class Index(name: String, unique: Boolean, key: String, f: JsObject => JsValue)


sealed trait Entity[E <: Entity[E]] extends EntityElement[E] with Neo4JElement { this: E => }
trait EntityBuilder[E <: Entity[E]] {
  def apply(jsValue:E#Js, indexes:Seq[Index]):E
}

case class Node(jsValue: JsObject, indexes: Seq[Index] = Nil) extends NodeElement with Entity[Node]
object Node {
  def apply(indexes: Seq[Index], data: (String, JsValue)*): Node =
    Node(
      JsObject(fields = Seq("data" -> JsObject(data.toSeq))),
      indexes = indexes
    )
}

case class Relation(jsValue: JsObject, indexes: Seq[Index] = Nil) extends RelationElement with Entity[Relation]
object Relation {
  def apply(start: Either[String, Node], end: Either[String, Node], rtype: String, indexes: Seq[Index], data: (String, JsValue)*): Relation =
    Relation(
      jsValue = JsObject(
        Seq(
          ("data" -> JsObject(fields = data.toSeq)),
          ("start" -> JsString(start.fold(s => s, n => n.self))),
          ("end" -> JsString(end.fold(s => s, n => n.self))),
          ("type" -> JsString(rtype))
        )
      ),
      indexes = indexes
    )
}

case class CypherResult(jsValue: JsObject) extends Neo4JElement {
  type Js = JsObject

  lazy val data = (jsValue \ "data").as[Seq[Seq[JsValue]]]
  lazy val columns = (jsValue \ "columns").as[Seq[String]]


  lazy val result:Seq[Seq[(String, JsValue)]] = data.map {columns zip _}

  lazy val resultAsMap:Seq[Map[String, JsValue]] = data.map {columns zip _ toMap}

  lazy val transposedResultAsMap:Map[String, Seq[JsValue]] = columns zip data.transpose toMap

  def apply(column:String):Seq[JsValue] = transposedResultAsMap(column)
  def opt(column:String):Option[Seq[JsValue]] = transposedResultAsMap.get(column)
}

case class Failure(jsValue: JsObject, status: Int, info: String) extends Exception with Neo4JElement {
  type Js = JsObject

  lazy val message = (jsValue \ "message").asOpt[String]
  lazy val exception = (jsValue \ "exception").as[String]

  lazy val stacktrace = (jsValue \ "stacktrace").as[List[String]]

  override def toString(): String = "Fails with status " + status + " and message " + message.getOrElse("<nothing>")

}

case class Empty() extends Neo4JElement {
  type Js = JsNull.type

  lazy val jsValue = JsNull
}



object Neo4JElement {

  type Aoutch = NonEmptyList[Exception]

  implicit def aoutch(e:Exception):Aoutch = NonEmptyList[Exception](e)

  implicit def wrapEitherT[E](f:Future[Aoutch \/ E]):EitherT[Future, Aoutch, E] = EitherT(f)

}