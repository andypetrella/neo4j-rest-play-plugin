package be.nextlab.play.neo4j.rest

import play.api.libs.json._
import play.api.Play
import play.api.libs.ws.WS
import play.api.libs.ws.WS._
import play.api.libs.concurrent.Promise
import com.ning.http.client.Realm.AuthScheme
import scalaz.{Failure => KO, Success => OK, _}
import scalaz.Scalaz._
import be.nextlab.play.neo4j.rest.Neo4JElement._
import ValidationPromised._


/**
 * User: andy
 */

case class Neo4JEndPoint(protocol: String, host: String, port: Int, credentials: Option[(String, String)]) {
  import Neo4JEndPoint._
  
  private lazy val serviceRootUrl = 
    request(Left(protocol + "://" + host + ":" + port)) acceptJson() get() map {
      resp =>
        resp.status match {
          case 200 => {
            resp.json match {
              case jo: JsObject => OK((jo \ "data").as[String])
              case r => KO(NonEmptyList("The base request must return a JsObject containing data and manage").left[Failure])
            }
          }
          case status => KO(NonEmptyList("The status is not ok " + status).left[Failure])
        }
    } transformer

  private[Neo4JEndPoint] def resolveFrom(from: Either[String, WSRequestHolder]): WSRequestHolder = 
    from match {
      case Left(s) => WS.url(s)
      case Right(rh) => rh
    }

  def request(from: Either[String, WSRequestHolder]) = 
    credentials map {
      t: (String, String) => resolveFrom(from).withAuth(t._1, t._2, AuthScheme.BASIC)
    } getOrElse resolveFrom(from)


  //In order to keep things using Promise, we use pure to create it after having waited for the root
  lazy val root:ValidationPromised[Aoutch, Root] = 
    Promise.pure((for {
      url <- serviceRootUrl;
      r <- request(Left(url)) acceptJson() get() map {
        resp =>
          resp.status match {
            case 200 => {
              resp.json match {
                case jo: JsObject => OK(Root(jo))
                case x => KO(NonEmptyList("Unexpected response while getting the root url : " + x).left[Failure])
              }
            }
            case status => KO(NonEmptyList("The status is not ok " + status).left[Failure])
          }
      } transformer
    } yield r).promised.await.get).transformer
}

case class WSRequestHolderW(w:WSRequestHolder) {

  def acceptJson():WSRequestHolder = w withHeaders ("Accept" -> "application/json")

}

object Neo4JEndPoint {
  implicit def wrapHolder(w:WSRequestHolder):WSRequestHolderW = WSRequestHolderW(w)
}

