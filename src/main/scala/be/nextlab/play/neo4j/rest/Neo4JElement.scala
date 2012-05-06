package be.nextlab.play.neo4j.rest

import scalaz.Monoid
import play.api.libs.json.Json._
import play.api.libs.concurrent.Promise
import play.api.libs.ws.WS.WSRequestHolder
import collection.Seq
import be.nextlab.play.neo4j.rest.Neo4JEndPoint._
import scala.Predef._
import java.lang.IllegalStateException
import play.api.libs.json._


/**
 * User: andy
 */
sealed abstract class Neo4JElement {
  type Js <: JsValue

  val jsValue: Js
}

case class Root(jsValue: JsObject) extends Neo4JElement {

  import Neo4JElement._

  type Js = JsObject

  lazy val neo4jVersion = (jsValue \ "neo4j_version").as[String]


  lazy val relationshipTypes = (jsValue \ "relationship_types").as[String]


  lazy val batch = (jsValue \ "batch").as[String]


  lazy val extensionsInfo = (jsValue \ "extensions_info").as[String]

  lazy val extensions = (jsValue \ "extensions").as[JsObject]


  //////REFERENCE/////
  lazy val _referenceNode = (jsValue \ "reference_node").as[String]

  def referenceNode(implicit neo: Neo4JEndPoint) = getNode(_referenceNode)


  //////NODE/////
  lazy val _node = (jsValue \ "node").as[String]

  def getNode(id: Int)(implicit neo: Neo4JEndPoint): Promise[Neo4JElement] = getNode(_node + "/" + id)

  def getNode(url: String)(implicit neo: Neo4JEndPoint): Promise[Neo4JElement] = neo.request(Left(url)) acceptJson() get() map {
    resp =>
      resp.status match {
        case 200 => resp.json match {
          case jo: JsObject => Node(jo)
          case _ => throw new IllegalStateException("Get Node must return a JsObject")
        }
        case 404 => resp.json match {
          case jo: JsObject => Failure(jo, 404, "Node not Found")
          case _ => throw new IllegalStateException("Get Node (errored) must return a JsObject")
        }
      }
  }

  def createNode(n: Option[Node])(implicit neo: Neo4JEndPoint): Promise[Neo4JElement] = {
    val holder: WSRequestHolder = neo.request(Left(_node)) acceptJson()

    (n match {
      case None => holder post (JsObject(Seq()))
      case Some(node) => holder post (node.data)
    }) map {
      resp =>
        resp.status match {
          case 201 => resp.json match {
            case jo: JsObject => Node(jo, n map {
              _.indexes
            } getOrElse (Nil))
            case _ => throw new IllegalStateException("Create Node must return a JsObject")
          }
        }
    } flatMap {
      case o: Node => o.applyIndexes map {
        //apply indexes at the end
        case f: Failure => {
          o.delete //WARN:: we delete because indexes has failed...
          f
        }
        case x => x
      }
      case _ => {
        throw new IllegalStateException("TODO")
      }
    }
  }

  def getUniqueNode(key: String, value: JsValue)(indexName: String)(implicit neo: Neo4JEndPoint): Promise[Option[Neo4JElement]] =
    neo.request(Left(_nodeIndex + "/" + indexName + "/" + key + "/" + jsToString(value))) get() map {
      resp => resp.status match {
        case 200 => resp.json match {
          case jo: JsObject => Some(Node(jo))
          case ja: JsArray => ja.value match {
            case Nil => None
            case (a: JsObject) :: Nil => Some(Node(a))
            case x => throw new IllegalStateException("Get UniqueNode must return a JsObject or a singleton array and not " + x)
          }
          case x => throw new IllegalStateException("Get Unique Node must return a JsObject or a singleton array and not " + x)
        }
        case 404 => resp.json match {
          case jo: JsObject => Some(Failure(jo, 404, "Unique Node not Found"))
          case x => throw new IllegalStateException("Get Unique Node (errored) must return a JsObject and not " + x)
        }
      }
    }

  //////CYPHER/////
  lazy val _cypher = (jsValue \ "cypher").as[String]

  def cypher(query: JsObject)(implicit neo: Neo4JEndPoint) =
    neo.request(Left(_cypher)) acceptJson() post (query) map {
      resp =>
        resp.status match {
          case 200 => resp.json match {
            case j: JsObject => CypherResult(j)
            case _ => throw new IllegalStateException("Get Node must return a JsObject")
          }
          case x if x == 400 => resp.json match {
            case j: JsObject => Failure(j, x, "Fail to execute cypher")
            case _ => throw new IllegalStateException("Not recognized failure")
          }
        }
    }

  //////NODE INDEXES/////
  lazy val _nodeIndex = (jsValue \ "node_index").as[String]
  //////RELATION INDEXES/////
  lazy val _relationshipIndex = (jsValue \ "relationship_index").as[String]


}

case class Index(name: String, unique: Boolean, key: String, f: JsObject => JsValue)

sealed trait Entity[E <: Entity[E]] extends Neo4JElement {
  this: E =>
  type Js = JsObject

  def indexes: Seq[Index]

  def build(j: JsObject, idxs: Seq[Index]): E

  def index(r: Root): String

  //url to it self
  lazy val self = (jsValue \ "self").as[String]
  lazy val id = self.substring(self.lastIndexOf('/') + 1).toInt

  //object holding properties
  lazy val data = (jsValue \ "data").as[JsObject]
  lazy val property = (jsValue \ "property").as[String]
  lazy val _properties = (jsValue \ "properties").as[String]

  lazy val extensions = (jsValue \ "extensions").as[JsObject]

  def updateData(data: (String, JsValue)*) =
    build(JsObject(("data" -> JsObject(data.toSeq)) +: this.jsValue.fields.filterNot(_._1 == "data")), this.indexes)


  def properties(data: Option[JsObject])(implicit neo: Neo4JEndPoint): Promise[Neo4JElement] =
    data match {

      case Some(d) => this.deleteFromIndexes flatMap {
        r => //indexes have been deleted => now update the properties
          neo.request(Left(_properties)) acceptJson() put (d) map {
            resp =>
              resp.status match {
                case 204 => this.updateData(d.fields: _*)
                case x => throw new IllegalStateException("TODO : update props error " + x + "(" + d + ")")
              }
          }
      } flatMap {
        case o: E => o.applyIndexes //apply indexes after update
        case _ => throw new IllegalStateException("TODO")
      }

      case None => neo.request(Left(_properties)) acceptJson() get() map {
        resp =>
          resp.status match {
            case 200 => resp.json match {
              case j: JsObject => this.updateData(j.fields: _*)
              case _ => throw new IllegalStateException("Get Properties for Entity must return a JsObject")
            }
            case x => throw new IllegalStateException("TODO : get props error " + x)
          }
      }
    }


  def applyIndexes(implicit neo: Neo4JEndPoint): Promise[Neo4JElement] = indexes.foldLeft(Promise.pure(this): Promise[Neo4JElement]) {
    (pr, idx) => pr flatMap {
      _ match {
        case f: Failure => Promise.pure(f.asInstanceOf[Neo4JElement])
        case _ => applyIndex(idx)
      }
    }
  }

  def applyIndex(idx: Index)(implicit neo: Neo4JEndPoint): Promise[Neo4JElement] =
    (for {
      r <- neo.root;
      i <- neo.request(Left(index(r) + "/" + idx.name + (if (idx.unique) "?unique" else ""))) post (JsObject(Seq("key" -> JsString(idx.key), "value" -> idx.f(jsValue), "uri" -> JsString(self))))
    } yield i) map {
      resp => resp.status match {
        case 201 => this //index value created
        case 200 => this //index value updated
      }
    }

  def deleteFromIndexes(implicit neo: Neo4JEndPoint): Promise[Neo4JElement] = indexes.foldLeft(Promise.pure(this): Promise[Neo4JElement]) {
    (pr, idx) => pr flatMap {
      _ => deleteFromIndex(idx)
    }
  }

  def deleteFromIndex(idx: Index)(implicit neo: Neo4JEndPoint): Promise[Neo4JElement] =
    (for {
      r <- neo.root;
      //todo ?? cannot do that because we cannot assert that the current entity is not already updated with new properties
      // d <- neo.request(Left(index(r) + "/" + idx._1 + "/" + idx._3 + "/" + jsToString(idx._4(jsValue)) + "/" + id)) acceptJson() delete()//too externalize
      d <- neo.request(Left(index(r) + "/" + idx.name + "/" + idx.key + "/" + id)) acceptJson() delete() //too externalize
    } yield d) map {
      resp => resp.status match {
        case 204 => this
      }
    }

  def delete(implicit neo: Neo4JEndPoint): Promise[Neo4JElement] = {
    //delete from indexes first
    deleteFromIndexes flatMap {
      _ =>
        neo.request(Left(self)) acceptJson() delete() map {
          resp =>
            resp.status match {
              case 204 => this
              case x if x == 409 => resp.json match {
                case jo: JsObject => Failure(jo, x, "Cannot Delete Entity with relations")
                case _ => throw new IllegalStateException("delete Entity must return a JsObject")
              }
              case x => throw new IllegalStateException("TODO : delete entity error " + x)
            }
        }
    }
  }

}

case class Node(jsValue: JsObject, indexes: Seq[Index] = Nil) extends Entity[Node] {

  import Neo4JElement._

  lazy val traverse = (jsValue \ "traverse").as[String]
  lazy val pagedTraverse = (jsValue \ "paged_traverse").as[String]

  lazy val allRelationships = (jsValue \ "all_relationships").as[String]
  lazy val allTypedRelationships = (jsValue \ "all_typed_relationships").as[String]
  lazy val incomingRelationships = (jsValue \ "incoming_relationships").as[String]
  lazy val incomingTypedRelationships = (jsValue \ "incoming_typed_relationships").as[String]
  lazy val outgoingRelationships = (jsValue \ "outgoing_relationships").as[String]

  lazy val _createRelationship = (jsValue \ "create_relationship").as[String]

  def createRelationship(r: Relation)(implicit neo: Neo4JEndPoint) =
    neo.request(Left(_createRelationship)) acceptJson() post (JsObject(Seq(
      "to" -> JsString(r._end),
      "type" -> JsString(r.`type`),
      "data" -> r.data
    ))) map {
      resp => resp.status match {
        case 201 => resp.json match {
          case o: JsObject => Relation(o, r.indexes)
          case x => throw new IllegalStateException("create relation must return a JsObject")
        }
      }
    }

  lazy val _outgoingTypedRelationships = (jsValue \ "outgoing_typed_relationships").as[String]

  def outgoingTypedRelationships(types: Seq[String])(implicit neo: Neo4JEndPoint) =
    neo.request(Left(_outgoingTypedRelationships.replace("{-list|&|types}", types.mkString("&")))) acceptJson() get() map {
      resp => resp.status match {
        case 200 => resp.json match {
          case o: JsArray => o.value.map {
            j => j match {
              case jo: JsObject => Relation(jo)
              case x => throw new IllegalStateException("get typed relations must return a JsArray o JsObject only")
            }
          }
          case x => throw new IllegalStateException("get typed relations must return a JsArray")
        }
      }
    }


  def ++(other: Node)(implicit m: Monoid[Node]) = m append(this, other)

  def build(j: JsObject, idxs: Seq[Index]) = Node(j, idxs)

  def index(r: Root) = r._nodeIndex

}

case class Relation(jsValue: JsObject, indexes: Seq[Index] = Nil) extends Entity[Relation] {

  import Neo4JElement._

  lazy val `type` = (jsValue \ "type").as[String]

  //START
  lazy val _start = (jsValue \ "start").as[String]

  def start(implicit neo: Neo4JEndPoint) = neo.root flatMap {
    _.getNode(_start)
  }

  //END
  lazy val _end = (jsValue \ "end").as[String]

  def end(implicit neo: Neo4JEndPoint) = neo.root flatMap {
    _.getNode(_end)
  }

  def ++(other: Relation)(implicit m: Monoid[Relation]) = m append(this, other)

  def build(j: JsObject, idxs: Seq[Index]) = Relation(j, idxs)

  def index(r: Root) = r._relationshipIndex

}

case class CypherResult(jsValue: JsObject) extends Neo4JElement {
  type Js = JsObject

  lazy val data = (jsValue \ "data").as[Seq[Seq[JsValue]]]
  lazy val columns = (jsValue \ "columns").as[Seq[String]]


  def result = data.map {
    one => {
      columns zip one
    }
  }
}

case class Failure(jsValue: JsObject, status: Int, info: String) extends Neo4JElement {
  type Js = JsObject


  lazy val message = (jsValue \ "message").as[String]
  lazy val exception = (jsValue \ "exception").as[String]

  lazy val stacktrace = (jsValue \ "stacktrace").as[List[String]]

}


case class Empty() extends Neo4JElement {
  type Js = JsNull.type

  lazy val jsValue = JsNull
}

object Neo4JElement {

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


object Node {

  import Neo4JElement._

  implicit object NodeMonoid extends Monoid[Node] {
    def append(s1: Node, s2: => Node) = {
      val newData: JsObject = (s1.data +++ s2.data) match {
        case o: JsObject => o
        case x => throw new IllegalStateException("Cannot add two nodes : " + s1 + " and " + s2)
      }
      val newFields: Seq[(String, JsValue)] = s1.jsValue.fields.filter(_._1 != "data") ++
        s2.jsValue.fields.filter(f => f._1 != "data" && s1.jsValue.fields.find(_._1 == f._1).isEmpty)

      val allNewFields: Seq[(String, JsValue)] = ("data", newData) +: newFields
      Node(JsObject(allNewFields), s1.indexes ++ (s2.indexes diff s1.indexes))
    }

    val zero = Node(JsObject(Seq("data" -> JsObject(Seq()))))
  }

  def apply(indexes: Seq[Index], data: (String, JsValue)*): Node =
    Node(
      JsObject(fields = Seq("data" -> JsObject(data.toSeq))),
      indexes = indexes
    )

}

object Relation {

  import Neo4JElement._

  implicit object RelationMonoid extends Monoid[Relation] {
    def append(s1: Relation, s2: => Relation) = {
      val newData: JsObject = (s1.data +++ s2.data) match {
        case o: JsObject => o
        case x => throw new IllegalStateException("Cannot add two relations : " + s1 + " and " + s2)
      }
      val newFields: Seq[(String, JsValue)] = s1.jsValue.fields.filter(_._1 != "data") ++
        s2.jsValue.fields.filter(f => f._1 != "data" && s1.jsValue.fields.find(_._1 == f._1).isEmpty)
      val allNewFields: Seq[(String, JsValue)] = ("data", newData) +: newFields
      Relation(JsObject(allNewFields), s1.indexes ++ (s2.indexes diff s1.indexes))
    }

    val zero = Relation(JsObject(Seq("data" -> JsObject(Seq()))))
  }

  def apply(start: Either[String, Node], end: Either[String, Node], `type`: String, indexes: Seq[Index], data: (String, JsValue)*): Relation =
    Relation(
      jsValue = JsObject(
        Seq(
          ("data" -> JsObject(fields = data.toSeq)),
          ("start" -> JsString(start.fold(s => s, n => n.self))),
          ("end" -> JsString(end.fold(s => s, n => n.self))),
          ("type" -> JsString(`type`))
        )
      ),
      indexes = indexes
    )

}
