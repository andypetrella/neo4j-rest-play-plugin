package be.nextlab.play.neo4j.rest

import play.api.Play.current

import play.api.Logger

import play.api.libs.json._
import play.api.libs.json.Json._

import play.api.libs.concurrent.Akka

import play.api.libs.ws.WS.WSRequestHolder

import be.nextlab.play.neo4j.rest.{Neo4JEndPoint => NEP}
import be.nextlab.play.neo4j.rest.Neo4JEndPoint._


import scalaz.Monoid
//import scalaz.Scalaz._

import scala.concurrent.{Future, ExecutionContext}

//Akkaz: implementation of Functor[Future] and Monad[Future]
//import scalaz.akkaz.future._

/**
 * User: andy
 */
trait EntityElement[E <: Entity[E]] {this:E =>

  import Neo4JElement._

//  implicit val executionContext = Akka.system.dispatcher

  type Js = JsObject

  def indexes: Seq[Index]

  def index(r: Root): String

  //url to it self
  lazy val self = (jsValue \ "self").as[String]
  lazy val id = self.substring(self.lastIndexOf('/') + 1).toInt

  //object holding properties
  lazy val data = (jsValue \ "data").as[JsObject]
  lazy val property = (jsValue \ "property").as[String]
  lazy val _properties = (jsValue \ "properties").as[String]

  lazy val extensions = (jsValue \ "extensions").as[JsObject]

  def updateData(data: (String, JsValue)*)(implicit builder:EntityBuilder[E]) =
    builder(JsObject(("data" -> JsObject(data.toSeq)) +: this.jsValue.fields.filterNot(_._1 == "data")), this.indexes)

  /**
   * PUT http://localhost:7474/db/data/node/7/properties/foo
   *Accept: application/json
   *Content-Type: application/json
   *"bar"
   *Example response
   *
   *204: No Content
   */
  def <+(key:String, value:Option[JsValue])(implicit neo: NEP, builder:EntityBuilder[E], ec:ExecutionContext) =
    value match {
      case Some(v) =>
        for {
          a <-  this.deleteFromIndexes; //TODO optimize
          u <-  neo.request(Left(_properties + "/" + key)) acceptJson() put(v) map {
                  withNotHandledStatus(Seq(204)) {
                    case JsNull => this.updateData(((key->v)+:this.data.fields.filterNot(_._1==key)):_*)
                  }
                }
          b <-  u.applyIndexes //reapply indexes after update
        } yield b

      //case None =>
    }

  def properties(data: Option[JsObject])(implicit neo: NEP, builder:EntityBuilder[E], ec:ExecutionContext): Future[E] =
    data match {

      case Some(d) =>
        for {
          a <-  this.deleteFromIndexes;
          u <-  neo.request(Left(_properties)) acceptJson() put(d) map {
                  withNotHandledStatus(Seq(204)) {
                    case JsNull => this.updateData(d.fields: _*)
                  }
                }
          b <- u.applyIndexes //apply indexes after update
        } yield b

      case None =>
        neo.request(Left(_properties)) acceptJson() get() map {
          withNotHandledStatus(Seq(200)) {
            case j: JsObject => this.updateData(j.fields: _*)
          }
        }
    }


  def applyIndexes(implicit neo: NEP, ec:ExecutionContext): Future[E] =
    indexes.foldLeft(Future.successful(this):Future[E]) {
      (pr, idx) => pr flatMap {x => x.applyIndex(idx)}
    }


  def applyIndex(idx: Index)(implicit neo: NEP, ec:ExecutionContext): Future[E] =
    for {
      r <-  neo.root;
      v <-  neo.request(
              Left(index(r) + "/" + idx.name + (if (idx.unique) "?unique" else ""))
            ) post (
              JsObject(Seq(
                "key" -> JsString(idx.key),
                "value" -> idx.f(jsValue),
                "uri" -> JsString(self)))
            ) map {
                withNotHandledStatus(Seq(200, 201)) {
                  case j: JsObject => this //index value created | updated
                }
            }
    } yield v


  def deleteFromIndexes(implicit neo: NEP, ec:ExecutionContext): Future[E] =
      indexes.foldLeft(Future.successful(this):Future[E]) {
        (pr, idx) => pr flatMap { x => x.deleteFromIndex(idx) }
      }

  def deleteFromIndex(idx: Index)(implicit neo: NEP, ec:ExecutionContext): Future[E] =
    for {
    r <- neo.root;
      //todo ?? cannot do that because we cannot assert that the current entity is not already updated with new properties
      // d <- neo.request(Left(index(r) + "/" + idx._1 + "/" + idx._3 + "/" + jsToString(idx._4(jsValue)) + "/" + id)) acceptJson() delete()//too externalize
    v <-  neo.request(
            Left(index(r) + "/" + idx.name + "/" + idx.key + "/" + id)
          ) acceptJson() delete() map {
            withNotHandledStatus(Seq(204)) {
              case JsNull => this //deleted
            }
          }
    } yield v

  def delete(implicit neo: NEP, ec:ExecutionContext): Future[E] =
    for {
      //delete from indexes first
      d <- deleteFromIndexes
      v <- neo.request(Left(self)) acceptJson() delete() map {
              withNotHandledStatus(Seq(204), Seq(409)) (
                {
                  case JsNull => this
                },
                {
                  case jo: JsObject => throw Failure.badStatus(jo, Seq(204), 409, "Cannot Delete Entity with relations")
                }
              )
          }
      } yield v

}