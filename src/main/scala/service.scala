package play.neo4j.rest

import scala.concurrent.{Future, ExecutionContext}

import play.api.libs.json.{Reads, JsObject}
import play.api.libs.ws.Response

trait Service  extends EndPoint with Version {
  import Service._

  implicit def ec:ExecutionContext

  def apply[T](q:Query[T]) = q(this)
}

object Service {
  type Query[T] = Service=>Future[T]
}

object Queries {
  import Service._
  import EndPoint._

  def byId[T](id:Int)(implicit r:Reads[T], ec:ExecutionContext):Query[Option[T]] =
    (service:Service) =>
      for {
        url <- service.url(id)
        n   <- service.request(url).get().handle[Option[T]]
                .json(200) {
                  case js:JsObject => r.reads(js).asOpt //todo toOption not true
                }
                .on(404) {
                  case resp:Response => None
                }.future
      } yield n
}

case class Service19M04(
  protocol: Protocol,
  host: String,
  port: Int,
  credentials: Option[(String, String)]
)(implicit val ec:ExecutionContext) extends Service with Version19M4

