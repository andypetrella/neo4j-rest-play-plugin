package be.nextlab.play.neo4j.rest

import play.api.Play.current

import play.api.Logger

import play.api.libs.json._
import play.api.libs.json.Json._

import play.api.libs.concurrent.Akka

import play.api.libs.ws.WS.WSRequestHolder

import be.nextlab.play.neo4j.rest.{Neo4JEndPoint => NEP}
import be.nextlab.play.neo4j.rest.Neo4JEndPoint._


import scalaz.{Failure => KO, Success => OK, _}
import scalaz.Scalaz._

import scala.concurrent.Promise
import scala.concurrent.Future

//Akkaz: implementation of Functor[Future] and Monad[Future]
import scalaz.akkaz.future._
/**
 * User: andy
 */
trait NodeElement {
  this:Node =>

  import Neo4JElement._
  import NodeElement._
  import JsValueHelper._

  lazy val traverse = (jsValue \ "traverse").as[String]
  lazy val pagedTraverse = (jsValue \ "paged_traverse").as[String]

  lazy val allTypedRelationships = (jsValue \ "all_typed_relationships").as[String]
  lazy val incomingRelationships = (jsValue \ "incoming_relationships").as[String]
  lazy val incomingTypedRelationships = (jsValue \ "incoming_typed_relationships").as[String]
  lazy val outgoingRelationships = (jsValue \ "outgoing_relationships").as[String]

  lazy val _createRelationship = (jsValue \ "create_relationship").as[String]

  def createRelationship(r: Relation)(implicit neo: NEP):EitherT[Future, Exception, Relation] =
    neo.request(Left(_createRelationship)) acceptJson() post (JsObject(Seq(
      "to" -> JsString(r._end),
      "type" -> JsString(r.rtype),
      "data" -> r.data
    ))) map {
      withNotHandledStatus(Seq(201)) {
        case o: JsObject => Relation(o, r.indexes).right
      }
    }

  lazy val _allRelationships = (jsValue \ "all_relationships").as[String]
  def allRelationships(implicit neo: NEP):EitherT[Future, Exception, Seq[Relation]] =
    neo.request(Left(_allRelationships)) acceptJson() get() map {
      withNotHandledStatus(Seq(200)) {
        case o: JsArray =>
          o.value.foldLeft(Seq().right:Exception \/ Seq[Relation]){
            (acc, j) => (acc, j) match {
              case (\/-(s), (jo:JsObject)) => (Relation(jo) +: s).right
              case x => new IllegalArgumentException("get typed relations must return a JsArray o JsObject only").left
            }
          }
      }
    }

  lazy val _outgoingTypedRelationships = (jsValue \ "outgoing_typed_relationships").as[String]
  def outgoingTypedRelationships(types: Seq[String])(implicit neo: NEP):EitherT[Future, Exception, Seq[Relation]] =
    neo.request(Left(_outgoingTypedRelationships.replace("{-list|&|types}", types.mkString("&")))) acceptJson() get() map {
      withNotHandledStatus(Seq(200)) {
          case o: JsArray => o.value.foldLeft(Seq().right:Exception \/ Seq[Relation]){
            (acc, j) => (acc, j) match {
              case (\/-(s), (jo:JsObject)) => (Relation(jo) +: s).right
              case x => new IllegalArgumentException("get typed relations must return a JsArray o JsObject only").left
            }
          }
      }
    }


  def ++(other: Node)(implicit m: Monoid[Node]) = m append(this, other)

  def index(r: Root) = r._nodeIndex

}

object NodeElement {
  import Neo4JElement._
  import JsValueHelper._

  implicit object NodeMonoid extends Monoid[Node] {
    def append(s1: Node, s2: => Node) = {
      val newData: JsObject = (s1.data +++ s2.data) match {
        case o: JsObject => o
        case x => throw new IllegalStateException("Cannot add two nodes : " + s1 + " and " + s2)
      }
      val newFields: Seq[(String, JsValue)] = s1.jsValue.fields.filter(_._1 != "data") ++
        s2.jsValue.fields.filter(f => f._1 != "data" && s1.jsValue.fields.find(_._1 == f._1).isEmpty)

      val allNewFields: Seq[(String, JsValue)] = ("data", newData) +: newFields
      Node(JsObject(allNewFields), s1.indexes ++ (s2.indexes diff s1.indexes))
    }

    val zero = Node(JsObject(Seq("data" -> JsObject(Seq()))))
  }

  implicit object NodeBuilder extends EntityBuilder[Node] {
    def apply(jsValue:JsObject, indexes:Seq[Index]):Node = Node(jsValue, indexes)
  }
}
