package be.nextlab.play.neo4j.rest

import play.api.libs.json._
import play.api.Play
import play.api.libs.ws.WS
import play.api.libs.ws.WS._
import play.api.libs.concurrent.Promise
import com.ning.http.client.Realm.AuthScheme

/**
 * User: andy
 */

case class Neo4JEndPoint(protocol: String, host: String, port: Int, credentials: Option[(String, String)]) {
  import Neo4JEndPoint._
  
  private lazy val serviceRootUrl = request(Left(protocol + "://" + host + ":" + port)) acceptJson() get() map {
    resp =>
      resp.status match {
        case 200 => {
          resp.json match {
            case jo: JsObject => (jo \ "data").as[String]
            case r => throw new IllegalStateException("The base request must return a JsObject containing data and manage")
          }
        }
        case status => throw new IllegalStateException("The status is not ok " + status)
      }
  }

  private[Neo4JEndPoint] def resolveFrom(from: Either[String, WSRequestHolder]): WSRequestHolder = from match {
    case Left(s) => WS.url(s)
    case Right(rh) => rh
  }

  def request(from: Either[String, WSRequestHolder]) = credentials map {
    t: (String, String) => resolveFrom(from).withAuth(t._1, t._2, AuthScheme.BASIC)
  } getOrElse resolveFrom(from)


  //In order to keep things using Promise, we use pure to create it after having waited for the root
  lazy val root:Promise[Root] = Promise.pure((serviceRootUrl flatMap {
    rootUrl => request(Left(rootUrl)) acceptJson() get() map {
      resp =>
        resp.status match {
          case 200 => {
            resp.json match {
              case jo: JsObject => Root(jo)
              case r => throw new IllegalStateException("The service root request must return a JsObject")
            }
          }
          case status => throw new IllegalStateException("The status is not ok " + status)
        }
    }
  }).await.get)
}

case class WSRequestHolderW(w:WSRequestHolder) {

  def acceptJson():WSRequestHolder = w withHeaders ("Accept" -> "application/json")

}

object Neo4JEndPoint {
  implicit def wrapHolder(w:WSRequestHolder):WSRequestHolderW = WSRequestHolderW(w)
}

