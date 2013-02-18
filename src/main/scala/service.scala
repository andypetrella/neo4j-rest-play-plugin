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

case class CQ() {
  import Start._

  def start(v:String) = new {
    def :=(s:Start) = StartingWith(v, s)
  }

}
case class Q(s:StartingWith, r:Return)

case class StartingWith(v:String, s:Start) {
  def retrn(r:Return) = Q(this, r)
}

//Starting points in the graph, obtained via index lookups or by element IDs.
case class Start(s:StartClause) {
  def matching:Option[Match] = None
}
object Start {
  def node(ids:Int*) = Start(NodeByIds(ids:_*))
  def node = new {
    def ~(i:IndexNodeClause) = Start(i)
  }
}
trait StartClause
case class NodeByIds(seq:Int*) extends StartClause
case class IndexNodeClause(name:String, ss:Seq[(String,String)]) extends StartClause
case class IndexNode(name:String) {
  def apply(ss:Seq[(String,String)]):IndexNodeClause = IndexNodeClause(name, ss)
}

//The graph pattern to match, bound to the starting points in START.
trait Match
// Filtering criteria.
trait Where
// What to return.
trait Return
case class Var(v:String) extends Return
// Creates nodes and relationships.
trait Create
// Removes nodes, relationships and properties.
trait Delete
// Set values to properties.
trait Set
// Performs updating actions once per element in a list.
trait Foreach
//Divides a query into multiple, distinct parts.
trait With

case class Service19M04(
  protocol: Protocol,
  host: String,
  port: Int,
  credentials: Option[(String, String)]
)(implicit val ec:ExecutionContext) extends Service with Version19M4

