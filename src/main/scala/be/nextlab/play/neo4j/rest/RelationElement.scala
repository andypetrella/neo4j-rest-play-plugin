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
trait RelationElement { this:Relation =>
  import Neo4JElement._
  import ValidationPromised._
  import RelationElement._
  import JsValueHelper._

  lazy val `type` = (jsValue \ "type").as[String]

  //START
  lazy val _start = (jsValue \ "start").as[String]

  def start(implicit neo: NEP) = neo.root /~~> { root => root.getNode(_start) }

  //END
  lazy val _end = (jsValue \ "end").as[String]

  def end(implicit neo: NEP) = neo.root /~~> { _.getNode(_end) }

  def ++(other: Relation)(implicit m: Monoid[Relation]) = m append(this, other)

  def index(r: Root) = r._relationshipIndex
}

object RelationElement {
  import Neo4JElement._
  import JsValueHelper._

  implicit object RelationMonoid extends Monoid[Relation] {
    def append(s1: Relation, s2: => Relation) = {
      val newData: JsObject = (s1.data +++ s2.data) match {
        case o: JsObject => o
        case x => throw new IllegalStateException("Cannot add two relations : " + s1 + " and " + s2)
      }
      val newFields: Seq[(String, JsValue)] = s1.jsValue.fields.filter(_._1 != "data") ++
        s2.jsValue.fields.filter(f => f._1 != "data" && s1.jsValue.fields.find(_._1 == f._1).isEmpty)
      val allNewFields: Seq[(String, JsValue)] = ("data", newData) +: newFields
      Relation(JsObject(allNewFields), s1.indexes ++ (s2.indexes diff s1.indexes))
    }

    val zero = Relation(JsObject(Seq("data" -> JsObject(Seq()))))
  }

  def apply(start: Either[String, Node], end: Either[String, Node], `type`: String, indexes: Seq[Index], data: (String, JsValue)*): Relation =
    Relation(
      jsValue = JsObject(
        Seq(
          ("data" -> JsObject(fields = data.toSeq)),
          ("start" -> JsString(start.fold(s => s, n => n.self))),
          ("end" -> JsString(end.fold(s => s, n => n.self))),
          ("type" -> JsString(`type`))
        )
      ),
      indexes = indexes
    )
  implicit object RelationBuilder extends EntityBuilder[Relation] {
    def apply(jsValue:JsObject, indexes:Seq[Index]):Relation = Relation(jsValue, indexes)
  }

}
