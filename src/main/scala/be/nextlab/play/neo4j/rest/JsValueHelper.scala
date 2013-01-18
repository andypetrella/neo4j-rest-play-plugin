package be.nextlab.play.neo4j.rest

import play.api.Logger

import play.api.libs.json._
import play.api.libs.json.Json._

import play.api.libs.concurrent.Promise

import play.api.libs.ws.WS.WSRequestHolder

import be.nextlab.play.neo4j.rest.{Neo4JEndPoint => NEP}
import be.nextlab.play.neo4j.rest.Neo4JEndPoint._


import scalaz.Monoid
//import scalaz.Scalaz._

/**
 * User: andy
 */
object JsValueHelper {

  implicit def jsToString(js: JsValue): String = js match {
    case o: JsObject => o.value.toString()
    case o: JsArray => o.value.view.map(jsToString(_)).mkString(",")
    case o: JsNull.type => ""
    case o: JsUndefined => o.error
    case o: JsNumber => o.value.toString()
    case o: JsBoolean => o.value.toString
    case o: JsString => o.value
  }

  trait JsValueW[Js <: JsValue] {
    def +++[T <: JsValue](me: Js, other: T): JsValue
  }


  implicit object JsObjectW extends JsValueW[JsObject] {
    def +++[T <: JsValue](me: JsObject, other: T): JsValue = other match {
      case o: JsObject => {
        val (incl, excl) = me.fields.foldLeft((Seq(), Seq()): (Seq[(String, (JsValue, JsValue))], Seq[(String, JsValue)])) {
          (acc, f) => {
            o.fields.find(_._1 == f._1) match {
              case Some(e) => ((f._1, (f._2, e._2)) +: acc._1, acc._2)
              case None => (acc._1, (f) +: acc._2)
            }
          }
        }
        val result: Seq[(String, JsValue)] = incl.foldLeft(Seq(): Seq[(String, JsValue)]) {
          (acc, p) => {
            p match {
              case (s: String, (j, k)) => (s, j +++ k) +: acc
              case _ => throw new IllegalStateException("Missing case ?!")
            }
          }
        } ++ excl ++ (o.fields.filter(of => me.fields.find(_._1 == of._1).isEmpty))
        JsObject(result)
      }
      case o if o == JsNull => me
      case o: JsUndefined => me
      case x => JsUndefined("Not tackled : " + me + "+++" + x)
    }
  }

  implicit object JsArrayW extends JsValueW[JsArray] {
    def +++[T <: JsValue](me: JsArray, other: T): JsValue = other match {
      case o: JsArray => me ++ o
      case o if o == JsNull => me
      case o: JsUndefined => me
      case x => JsUndefined("Not tackled : " + me + "+++" + x)
    }
  }

  implicit object JsStringW extends JsValueW[JsString] {
    def +++[T <: JsValue](me: JsString, other: T): JsValue = other match {
      case o: JsObject => JsString(me.value + o.value.toString())
      case o: JsArray => JsString(me.value + o.value.mkString(""))
      case o if o == JsNull => me
      case o: JsUndefined => me
      case o: JsNumber => JsString(me.value + o.value)
      case o: JsBoolean => JsString(me.value + o.value)
      case o: JsString => JsString(me.value + o.value)
      case x => JsUndefined("Not tackled : " + me + "+++" + x)
    }
  }

  implicit object JsNumberW extends JsValueW[JsNumber] {
    def +++[T <: JsValue](me: JsNumber, other: T): JsValue = other match {
      case o if o == JsNull => me
      case o: JsUndefined => me
      case o: JsNumber => JsNumber(me.value + o.value)
      case x => JsUndefined("Not tackled : " + me + "+++" + x)
    }
  }

  implicit object JsBooleanW extends JsValueW[JsBoolean] {
    def +++[T <: JsValue](me: JsBoolean, other: T): JsValue = other match {
      case o if o == JsNull => me
      case o: JsUndefined => me
      case o: JsBoolean => JsBoolean(me.value | o.value)
      case x => JsUndefined("Not tackled : " + me + "+++" + x)
    }
  }

  implicit object JsUndefinedW extends JsValueW[JsUndefined] {
    def +++[T <: JsValue](me: JsUndefined, other: T): JsValue = other match {
      case o: JsValue => o
      case x => JsUndefined("Not tackled : " + me + "+++" + x)
    }
  }

  implicit object JsNullW extends JsValueW[JsNull.type] {
    def +++[T <: JsValue](me: JsNull.type, other: T): JsValue = other match {
      case o if o == JsNull => JsNull
      case x => x
    }
  }

  implicit object JsValueMonoid extends Monoid[JsValue] {
    def append(s1: JsValue, s2: => JsValue) = s1 match {
      case o: JsNull.type => implicitly[JsValueW[JsNull.type]] +++(o, s2)
      case o: JsUndefined => implicitly[JsValueW[JsUndefined]] +++(o, s2)
      case o: JsString => implicitly[JsValueW[JsString]] +++(o, s2)
      case o: JsNumber => implicitly[JsValueW[JsNumber]] +++(o, s2)
      case o: JsBoolean => implicitly[JsValueW[JsBoolean]] +++(o, s2)
      case o: JsArray => implicitly[JsValueW[JsArray]] +++(o, s2)
      case o: JsObject => implicitly[JsValueW[JsObject]] +++(o, s2)
    }

    val zero = JsNull
  }

  case class JsValueAppendable(js: JsValue) {

    import JsValueMonoid._

    def +++(other: JsValue)(implicit m: Monoid[JsValue]): JsValue = m.append(js, other)
  }

  implicit def jsValueToAppendable(js: JsValue): JsValueAppendable = JsValueAppendable(js)

}