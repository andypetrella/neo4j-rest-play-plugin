package play.neo4j.rest

import play.api.Play.current

import play.api.libs.concurrent.Akka

import play.api.libs.json._

import play.api.libs.ws.Response
import play.api.libs.ws.WS
import play.api.libs.ws.WS._

import com.ning.http.client.Realm.AuthScheme

import scala.concurrent.{Future, ExecutionContext}

sealed trait Protocol {
  def prefix:String
}
case object HTTP extends Protocol {
  val prefix = "http"
  override def toString = prefix
  def unapply(s:String) = if (s == prefix) Some(s) else None
}

case object HTTPS extends Protocol {
  val prefix = "https"
  override def toString = prefix
  def unapply(s:String) = if (s == prefix) Some(s) else None
}
object Protocol {
  def apply(p:String):Protocol = p match {
    case HTTP(p) => HTTP
    case HTTPS(p) => HTTPS
  }
}


trait EndPoint { this:Service =>
  import EndPoint._

  def protocol: Protocol
  def host: String
  def port: Int
  def credentials: Option[(String, String)]

  val baseUrl = protocol + "://" + host + ":" + port

  private[EndPoint] def getRoot:Future[Root] =
    (for {
      data <- request(baseUrl).get().handle[String].json(200) {
                case jo:JsObject => (jo \ "data").as[String]
              }.future
      r    <-  request(data).get().handle[Root].json(200) {
                case jo:JsObject =>
                  rootFormat.reads(jo).fold(
                    invalid = x => throw new IllegalStateException("Unable to read JSON body to Root " + x),
                    valid   = a => a
                  )
              }.future
    } yield r)
    .recoverWith {
      case x:Throwable => {
        x.printStackTrace
        //retry...
        getRoot
      }
    }

  lazy val root = getRoot


  //compute a WS call with authentication and JSON encoding headers
  def request(url: String):WSRequestHolder =
    credentials
      .map{ case (u, p) =>
        WS.url(url).withAuth(u, p, AuthScheme.BASIC)
      } // Authenticated
      .getOrElse(WS.url(url)) // Anonymous
      .withHeaders( // JSON sent, JSON received
        "Accept"            -> "application/json",
        "Content-Type"      -> "application/json; charset=utf-8",
        "Accept-Charset"    -> "utf-8",
        "Content-Encoding"  -> "utf-8"
      )
}

object EndPoint {
  implicit class ToNeo4J(fr:Future[Response]) {
    def handle[T] = Neo4JResponse[T](fr)
  }

  case class Neo4JResponse[T](fr:Future[Response], handles:Option[PartialFunction[Response, T]]=None) {
    private[this] def orElse(f:Option[PartialFunction[Response, T]], g:PartialFunction[Response, T]) =
      f.map(_ orElse g) orElse Some(g)

    def interceptStatuses(f:PartialFunction[Int, Response => T]) =
      this.copy(handles =
        orElse(handles, {
          case resp:Response if f.isDefinedAt(resp.status) => f(resp.status)(resp)
        })
      )


    def on(status:Int)(f:Response => T) =
      interceptStatuses({
        case i:Int if i == status => f
      })

    def json(status:Int)(f:PartialFunction[JsValue, T])(implicit r:Reads[T]) =
      interceptStatuses({
        case i:Int if i == status => (resp:Response) => {
          val js = resp.json
          if (f.isDefinedAt(js)) {
            f(js)
          } else {
            throw BadJson(js, f)
          }
        }
      })


    def future(implicit ec:ExecutionContext) :Future[T] =
      fr.map { resp =>
        handles.map { hs =>
          if (hs.isDefinedAt(resp)) {
            hs(resp)
          } else {
            throw BadStatus(resp, hs)
          }
        }.getOrElse(throw new IllegalStateException("Missing handles"))
      }
  }
}