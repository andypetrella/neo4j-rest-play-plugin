package be.nextlab.play.neo4j.rest

import play.api.Play

import play.api.libs.json._

import play.api.libs.ws.Response
import play.api.libs.ws.WS
import play.api.libs.ws.WS._

import scala.concurrent.stm._


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
              case r => KO(NonEmptyList[Exception](new IllegalArgumentException("The base request must return a JsObject containing data and manage")))
            }
          }
          case status => KO(NonEmptyList(new IllegalStateException("The status is not ok " + status)))
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



  /**
   * Method to get the Neo4J root async
   */
  private[this] def getRoot =
    for {
      url <- serviceRootUrl;
      r <- request(Left(url)) acceptJson() get() map {
        resp =>
          resp.status match {
            case 200 => {
              resp.json match {
                case jo: JsObject => OK(Root(jo))
                case x => KO(NonEmptyList[Exception](new IllegalArgumentException("Unexpected response while getting the root url : " + x)))
              }
            }
            case status => KO(NonEmptyList[Exception](new IllegalStateException("The status is not ok " + status)))
          }
      } transformer
    } yield r

  /**
   * STM ref to a validation promised to the Root. This particular type is kept for further monadic usages
   */
  private[this] val r00t:Ref.View[Option[ValidationPromised[Aoutch, Root]]] = Ref(None:Option[ValidationPromised[Aoutch, Root]]).single

  /**
   * this methods helps in waiting for Neo4J Root request
   */
  private[this] def getR00t =
    try{
      Some(Promise.pure(getRoot.promised.await.get).transformer)
    } catch {
      case x => {
        x.printStackTrace
        None
      }
    }

  /**
   * Holds a reference to the Neo4J server root, it will try to fetch it in the cases:
   *  - it hasn't requested yet
   *  - it has already failed
     */
  lazy val root:ValidationPromised[Aoutch, Root] =
      r00t()
        .getOrElse(
          r00t.transformAndGet(_ => getR00t)
            .getOrElse(throw new IllegalStateException("Cannot connect to Neo4J"))
        )

}

case class WSRequestHolderW(w:WSRequestHolder) {

  def acceptJson():WSRequestHolder = w withHeaders (
    "Accept" -> "application/json",
    "Content-Type"->"application/json; charset=utf-8",
    "Accept-Charset"->"utf-8",
    "Content-Encoding"->"utf-8"
  )

}

case class WSResponseW(w:Response) {
  import scala.xml._

  def encBody = w.header("Content-Encoding").map{enc => w.getAHCResponse.getResponseBody(enc)}.getOrElse(w.body)
  def encJson: JsValue = Json.parse(encBody)
  def encXml: Elem = XML.loadString(encBody)
}

object Neo4JEndPoint {
  implicit def wrapHolder(w:WSRequestHolder):WSRequestHolderW = WSRequestHolderW(w)
  implicit def wrapResponse(r:Response):WSResponseW = WSResponseW(r)
}

