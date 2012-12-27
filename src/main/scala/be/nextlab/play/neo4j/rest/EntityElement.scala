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
trait EntityElement[E <: Entity[E]] {this:E =>

  import Neo4JElement._
  import ValidationPromised._

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
  def <+(key:String, value:Option[JsValue])(implicit neo: NEP, builder:EntityBuilder[E]) =
    value match {
      case Some(v) =>
        for {
          a <- this.deleteFromIndexes; //TODO optimize
          u <- neo.request(Left(_properties + "/" + key)) acceptJson() put(v) map { resp =>
                  /*indexes have been deleted => now update the properties*/
                  resp.status match {
                    case 204 => OK(this.updateData((key->v)))
                    case x => KO(NonEmptyList[Exception](new IllegalStateException("TODO : update prop error " + x + "(" + key + "," + v + ")")))
                  }
              } transformer;
          b <- u.applyIndexes //reapply indexes after update
        } yield b

      //case None =>
    }

  def properties(data: Option[JsObject])(implicit neo: NEP, builder:EntityBuilder[E]): ValidationPromised[Aoutch, E] =
    data match {

      case Some(d) => for {
          a <- this.deleteFromIndexes;
          u <- neo.request(Left(_properties)) acceptJson() put(d) map { resp =>
                  /*indexes have been deleted => now update the properties*/
                  resp.status match {
                    case 204 => OK(this.updateData(d.fields: _*))
                    case x => KO(NonEmptyList[Exception](new IllegalStateException("TODO : update props error " + x + "(" + d + ")")))
                  }
              } transformer;
          b <- u.applyIndexes //apply indexes after update
        } yield b

      case None => neo.request(Left(_properties)) acceptJson() get() map {
        resp =>
          resp.status match {
            case 200 => resp.encJson match {
              case j: JsObject => OK(this.updateData(j.fields: _*))
              case _ => KO(NonEmptyList[Exception](new IllegalArgumentException("Get Properties for Entity must return a JsObject")))
            }
            case x => KO(NonEmptyList[Exception](new IllegalStateException("TODO : get props error " + x)))
          }
      } transformer
    }


  def applyIndexes(implicit neo: NEP): ValidationPromised[Aoutch, E] =
    indexes.foldLeft(ValidationPromised(Promise.pure(OK(this))):ValidationPromised[Aoutch, E]) {
      (pr, idx) => pr /~~> {x => x.applyIndex(idx)}
    }


  def applyIndex(idx: Index)(implicit neo: NEP): ValidationPromised[Aoutch, E] =
    for {
      r <- neo.root;
      v <- neo.request(
          Left(index(r) + "/" + idx.name + (if (idx.unique) "?unique" else ""))
        ) post (
          JsObject(Seq(
            "key" -> JsString(idx.key),
            "value" -> idx.f(jsValue),
            "uri" -> JsString(self)))
        ) map { resp =>
          resp.status match {
            case x if x == 201 || x == 200 => OK(this) //index value created | updated
            case x => KO(NonEmptyList[Exception](new IllegalStateException("Cannot apply index : got status " + x)))
          }
        } transformer
    } yield v


  def deleteFromIndexes(implicit neo: NEP): ValidationPromised[Aoutch, E] =
      indexes.foldLeft(ValidationPromised(Promise.pure(OK(this))):ValidationPromised[Aoutch, E]) {
        (pr, idx) => pr /~~> { x => x.deleteFromIndex(idx) }
      }

  def deleteFromIndex(idx: Index)(implicit neo: NEP): ValidationPromised[Aoutch, E] =
    for {
    r <- neo.root;
      //todo ?? cannot do that because we cannot assert that the current entity is not already updated with new properties
      // d <- neo.request(Left(index(r) + "/" + idx._1 + "/" + idx._3 + "/" + jsToString(idx._4(jsValue)) + "/" + id)) acceptJson() delete()//too externalize
    v <-neo.request(
          Left(index(r) + "/" + idx.name + "/" + idx.key + "/" + id)
        ) acceptJson() delete() map { resp =>
          resp.status match {
            case 204 => OK(this) //deleted
            case x => KO(NonEmptyList[Exception](new IllegalStateException("Cannot delete from index : got status " + x)))
          }
        } transformer
    } yield v

  def delete(implicit neo: NEP): ValidationPromised[Aoutch, E] =
    for {
      //delete from indexes first
      d <- deleteFromIndexes
      v <- neo.request(Left(self)) acceptJson() delete() map {
            resp =>
              resp.status match {
                case 204 => OK(this)
                case x if x == 409 => resp.encJson match {
                  case jo: JsObject => KO(NonEmptyList(Failure(jo, x, "Cannot Delete Entity with relations")))
                  case _ => KO(NonEmptyList(new IllegalStateException("delete Entity must return a JsObject")))
                }
                case x => {
                  Logger.error("Error "+x+" for delete")
                  Logger.debug("Response Body:\n" + resp.body)
                  KO(NonEmptyList(new IllegalStateException("TODO : delete entity error " + x)))
                }
              }
          } transformer
      } yield v

}