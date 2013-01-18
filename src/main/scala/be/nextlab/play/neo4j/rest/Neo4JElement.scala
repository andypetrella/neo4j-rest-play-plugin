package be.nextlab.play.neo4j.rest

import play.api.Logger

import play.api.libs.json._
import play.api.libs.json.Json._

import play.api.libs.concurrent.Promise

import play.api.libs.ws.Response
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

trait Failure extends Exception with Neo4JElement {
  type Js = JsObject

  def info:String
  def status:Int

  lazy val message = (jsValue \ "message").asOpt[String]
  lazy val exception = (jsValue \ "exception").as[String]

  lazy val stacktrace = (jsValue \ "stacktrace").as[List[String]]

  override def toString(): String = "Fails with status "+status + " and message " + message.getOrElse("<no-message>") + " and also " + info

}
trait BadStatus extends Failure {
  def expectedStatuses:Seq[Int]
}

case class NotFound(jsValue:JsObject, expectedStatuses:Seq[Int], info:String = "<no-info>") extends BadStatus {
  def status:Int = 404
}

object Failure {
  def badStatus(jsonValue:JsObject, expected:Seq[Int], resultStatus:Int, extraInfo:String = "<no-info>") =
    resultStatus match {
      case 404 => NotFound(jsonValue, expected, extraInfo)
      case x =>
        new BadStatus {
          val jsValue = jsonValue
          val info = extraInfo
          val status = x
          val expectedStatuses = expected
        }
    }
}


case object Empty extends Neo4JElement {
  type Js = JsNull.type

  lazy val jsValue = JsNull
}



object Neo4JElement {
  // implicit class RecoverResult[T](e:EitherT[Future, Exception, T]) {
  //   def recover[U>:T](f:Exception => U)(implicit ec:ExecutionContext):EitherT[Future, Exception, U] = e.validationed { v =>
  //     v.fold(
  //       ex => f(ex).right,
  //       s => s.right
  //     ).validation
  //   }
  // }
  //implicit def wrapEitherT[E](f:Future[Exception \/ E]):EitherT[Future, Exception, E] = EitherT(f)


  def defaultNotHandler[T]:PartialFunction[JsValue,Exception]=
    {
      case js:JsValue => new IllegalStateException("This status should be handled but not handler was given")
    }

  def withNotHandledStatus[T](expected:Seq[Int], errors:Seq[Int] = Seq.empty)(f:PartialFunction[JsValue,T], g:PartialFunction[JsValue,Exception]=defaultNotHandler[T]):Response => T =
    (resp:Response) => {
      val status = resp.status
      lazy val js:JsValue = try {
        resp.json
      } catch {
        case x:Throwable => {
          Logger.info("Unable to parse json! Should be handled by the continuation partial function => So returning JsNull")
          JsNull
        }
      }

      if (expected.contains(status)) {
        if (f.isDefinedAt(js)) {
          f(js)
        } else {
          throw new IllegalArgumentException("Handled error ("+status+") but bad JSON encoding: " + js)
        }
      } else if (errors.contains(resp.status)) {
        if (g.isDefinedAt(js)) {
          throw g(js)
        } else {
          throw new IllegalArgumentException("Handled error ("+status+") but bad JSON encoding: " + js)
        }
      } else {
        js match {
          case jo: JsObject => throw Failure.badStatus(jo, expected, resp.status)
          case _ => throw new IllegalStateException("Request with status ("+resp.status+") must return a JsObject")
        }
      }
    }


}