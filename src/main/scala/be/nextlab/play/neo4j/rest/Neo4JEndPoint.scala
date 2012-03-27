package be.nextlab.play.neo4j.rest

import dispatch._
import play.api.libs.json._
import be.nextlab.play.neo4j.rest.util.PlayJsonDispatch._
import play.api.Play

/**
 * User: andy
 */

case class Neo4JEndPoint(host: String, port: Int, credentials: Option[(String, String)]) {
  private lazy val serviceRootUrl = Http(request(Right(:/(host, port))) <:< Map("Accept" -> "application/json") |>! {
    {
      case j: JsObject => (j \ "data").as[String]
      case _ => throw new IllegalStateException("The base request must return a JsObject containing data and manage")
    }
  })

  private[Neo4JEndPoint] def resolveFrom(from: Either[String, Request]): Request = from match {
    case Left(s) => url(s)
    case Right(r) => r
  }

  def request(from: Either[String, Request]) = credentials map {
    t: (String, String) => resolveFrom(from).as(t._1, t._2)
  } getOrElse resolveFrom(from)



  lazy val root = Http(request(Left(serviceRootUrl)) <:< Map("Accept" -> "application/json") |>! {
    {
      case rj: JsObject => Root(rj)
      case _ => throw new IllegalStateException("The service root request must return a JsObject")
    }
  })

}

