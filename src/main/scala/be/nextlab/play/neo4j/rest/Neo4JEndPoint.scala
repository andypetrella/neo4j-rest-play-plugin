package be.nextlab.play.neo4j.rest

import play.api.Play.current

import play.api.libs.concurrent.Akka

import play.api.libs.json._

import play.api.libs.ws.Response
import play.api.libs.ws.WS
import play.api.libs.ws.WS._

import scala.concurrent.stm._

import com.ning.http.client.Realm.AuthScheme

import scalaz.Monoid
// import scalaz.Scalaz._

import be.nextlab.play.neo4j.rest.Neo4JElement._

import scala.concurrent.{Future, Await, ExecutionContext}
import scala.concurrent.duration._

//Akkaz: implementation of Functor[Future] and Monad[Future]
//import scalaz.akkaz.future._

/**
 * User: andy
 */
case class Neo4JEndPoint(protocol: String, host: String, port: Int, credentials: Option[(String, String)]) {
  import Neo4JEndPoint._

  //implicit val executionContext = Akka.system.dispatcher

  private def serviceRootUrl(implicit ec:ExecutionContext) =
    request(Left(protocol + "://" + host + ":" + port)) acceptJson() get() map {
      withNotHandledStatus(Seq(200)) {
        case jo: JsObject => (jo \ "data").as[String]
      }
    }

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
  private[this] def getRoot(implicit ec:ExecutionContext) =
    for {
      url <-  serviceRootUrl;
      r   <-  request(Left(url)) acceptJson() get() map {
                withNotHandledStatus(Seq(200)) {
                  case jo: JsObject => Root(jo)
                }
              }
    } yield r

  /**
   * STM ref to a validation promised to the Root. This particular type is kept for further monadic usages
   */
  private[this] val r00t:Ref.View[Option[Future[Root]]] = Ref(None:Option[Future[Root]]).single

  /**
   * this methods helps in waiting for Neo4J Root request
   */
  private[this] def getR00t(implicit ec:ExecutionContext) =
    try{
      Some(Future.successful(Await.result(getRoot, 1 second)))
    } catch {
      case x:Throwable => {
        x.printStackTrace
        None
      }
    }


  /**
   * Holds a reference to the Neo4J server root, it will try to fetch it in the cases:
   *  - it hasn't requested yet
   *  - it has already failed
   * THIS HAS TO BE REWORKED !!!! because it's crappy!
  */
  def root(implicit ec:ExecutionContext):Future[Root] = {
    r00t()
      .getOrElse(
        r00t.transformAndGet(_ => getR00t)
          .getOrElse(throw new IllegalStateException("Cannot connect to Neo4J"))
      )

  }

}

case class WSRequestHolderW(w:WSRequestHolder) {

  def acceptJson():WSRequestHolder = w withHeaders (
    "Accept"            -> "application/json",
    "Content-Type"      -> "application/json; charset=utf-8",
    "Accept-Charset"    -> "utf-8",
    "Content-Encoding"  -> "utf-8"
  )

}

object Neo4JEndPoint {
  implicit def wrapHolder(w:WSRequestHolder):WSRequestHolderW = WSRequestHolderW(w)
}

