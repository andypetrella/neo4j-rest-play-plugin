package play.neo4j.rest

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.json.Json._
//import java.text.ParseException
//import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._

trait Version { this:Service =>
  def versionID:String
  def url(id:Int):Future[String]
  implicit def rootFormat:Format[Root]
  implicit def nodeFormat:Format[Node]
  def relationshipFormat:Format[Relationship]
}

trait Version19M4 extends Version { this:Service =>
  val versionID = "19.M04"
  def url(id:Int):Future[String] = root.map(_.node + "/"+id)
  implicit val rootFormat:Format[Root] =
    (
      (__ \ 'neo4j_version).format[String]
      and
      (__ \ 'node).format[String]
    )(Root, unlift(Root.unapply))
  implicit val nodeFormat:Format[Node] =
    (
      (__ \ 'self).format[String]
      and
      (__ \ 'data).format[JsObject]
    )(Node, unlift(Node.unapply))

  val relationshipFormat = null.asInstanceOf[Format[Relationship]]
}

