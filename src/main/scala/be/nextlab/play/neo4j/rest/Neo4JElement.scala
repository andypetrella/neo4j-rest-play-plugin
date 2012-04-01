package be.nextlab.play.neo4j.rest

import scalaz.Monoid
import play.api.libs.json._
import play.api.libs.json.Json._
import Neo4JEndPoint._
import play.api.libs.concurrent.Promise
import play.api.libs.ws.WS.WSRequestHolder
import collection.Seq


/**
 * User: andy
 */
sealed abstract class Neo4JElement {
  val jsValue: JsValue
}

case class Root(jsValue: JsObject) extends Neo4JElement {

  lazy val neo4jVersion = (jsValue \ "neo4j_version").as[String]


  lazy val relationshipTypes = (jsValue \ "relationship_types").as[String]

  lazy val nodeIndex = (jsValue \ "node_index").as[String]
  lazy val relationshipIndex = (jsValue \ "relationship_index").as[String]

  lazy val batch = (jsValue \ "batch").as[String]


  lazy val extensionsInfo = (jsValue \ "extensions_info").as[String]

  lazy val extensions = (jsValue \ "extensions").as[JsObject]

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
        }
    }


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

  def createNode(n: Option[Node])(implicit neo: Neo4JEndPoint) = {
    val holder: WSRequestHolder = neo.request(Left(_node)) acceptJson()

    (n match {
      case None => holder post(JsObject(Seq()))
      case Some(node) => holder post (node.data)
    }) map {
      resp =>
        resp.status match {
          case 201 => resp.json match {
            case jo: JsObject => Node(jo)
            case _ => throw new IllegalStateException("Create Node must return a JsObject")
          }
        }
    }
  }

}

case class Node(jsValue: JsObject) extends Neo4JElement {
  import Neo4JElement._
  import Node._

  type Js = JsObject

  //url to it self
  lazy val self = (jsValue \ "self").as[String]
  lazy val id = self.substring(self.lastIndexOf('/') + 1).toInt

  //object holding properties
  lazy val data = (jsValue \ "data").as[JsObject]

  lazy val traverse = (jsValue \ "traverse").as[String]
  lazy val pagedTraverse = (jsValue \ "paged_traverse").as[String]

  lazy val allRelationships = (jsValue \ "all_relationships").as[String]
  lazy val allTypedRelationships = (jsValue \ "all_typed_relationships").as[String]
  lazy val incomingRelationships = (jsValue \ "incoming_relationships").as[String]
  lazy val incomingTypedRelationships = (jsValue \ "incoming_typed_relationships").as[String]
  lazy val outgoingRelationships = (jsValue \ "outgoing_relationships").as[String]
  lazy val outgoingTypedRelationships = (jsValue \ "outgoing_typed_relationships").as[String]

  lazy val createRelationship = (jsValue \ "create_relationship").as[String]

  lazy val property = (jsValue \ "property").as[String]

  lazy val _properties = (jsValue \ "properties").as[String]

  def properties(data: Option[JsObject])(implicit neo: Neo4JEndPoint): Promise[Neo4JElement] = data match {

    case Some(d) => neo.request(Left(_properties)) acceptJson() put (d) map { resp =>
      resp.status match {
        case 204 => Node(JsObject(("data" -> d) +: this.jsValue.fields.filterNot(_._1 == "data")))
        case x => throw new IllegalStateException("TODO : update props error " + x + "(" + d + ")")
      }
    }

    case None => neo.request(Left(_properties)) acceptJson() get() map {
      resp =>
        resp.status match {
          case 200 => resp.json match {
            case j: JsObject => Node(JsObject(("data" -> j) +: this.jsValue.fields.filterNot(_._1 == "data")))
            case _ => throw new IllegalStateException("Get Properties Node must return a JsObject")
          }
          case x => throw new IllegalStateException("TODO : get props error " + x)
        }
    }
  }

  def delete(implicit neo: Neo4JEndPoint): Promise[Neo4JElement] =  {
    neo.request(Left(self)) acceptJson() delete() map { resp =>
      resp.status match {
        case 204 => this
        case 409 => resp.json match {
          case jo:JsObject => Failure(jo, 409, "Cannot Delete Node with relations")
          case _ => throw new IllegalStateException("delete Node must return a JsObject")
        }
        case x => throw new IllegalStateException("TODO : delete node error " + x)
      }
    }
  }


  lazy val extensions = (jsValue \ "extensions").as[JsObject]

  def ++(other:Node) = NodeMonoid append (this, other)

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

case class Failure(jsValue: JsObject, status:Int, info:String) extends Neo4JElement {
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

  trait JsValueW[Js <: JsValue] {
    def +++[T <: JsValue](me:Js, other: T):JsValue
  }
  
  implicit object JsObjectW extends JsValueW[JsObject] {
    def +++[T <: JsValue](me:JsObject, other: T):JsValue = other match {
      case o:JsObject => {
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
      case o if o == JsNull =>  me
      case o:JsUndefined => me
      case x => JsUndefined("Not tackled : " + me + "+++" + x)
    }
  }

  implicit object JsArrayW extends JsValueW[JsArray] {
    def +++[T <: JsValue](me:JsArray, other: T):JsValue = other match {
      case o:JsArray => me ++ o
      case o if o == JsNull =>  me
      case o:JsUndefined => me
      case x => JsUndefined("Not tackled : " + me + "+++" + x)
    }
  }

  implicit object JsStringW extends JsValueW[JsString] {
    def +++[T <: JsValue](me:JsString, other: T):JsValue = other match {
      case o:JsObject => JsString(me.value + o.value.toString())
      case o:JsArray => JsString(me.value + o.value.mkString(""))
      case o if o == JsNull =>  me
      case o:JsUndefined => me
      case o:JsNumber => JsString(me.value + o.value)
      case o:JsBoolean => JsString(me.value + o.value)
      case o:JsString => JsString(me.value + o.value)
      case x => JsUndefined("Not tackled : " + me + "+++" + x)
    }
  }

  implicit object JsNumberW extends JsValueW[JsNumber] {
    def +++[T <: JsValue](me:JsNumber, other: T):JsValue = other match {
      case o if o == JsNull =>  me
      case o:JsUndefined => me
      case o:JsNumber => JsNumber(me.value + o.value)
      case x => JsUndefined("Not tackled : " + me + "+++" + x)
    }
  }

  implicit object JsBooleanW extends JsValueW[JsBoolean] {
    def +++[T <: JsValue](me:JsBoolean, other: T):JsValue = other match {
      case o if o == JsNull =>  me
      case o:JsUndefined => me
      case o:JsBoolean => JsBoolean(me.value | o.value)
      case x => JsUndefined("Not tackled : " + me + "+++" + x)
    }
  }

  implicit object JsUndefinedW extends JsValueW[JsUndefined] {
    def +++[T <: JsValue](me:JsUndefined, other: T):JsValue = other match {
      case o:JsValue => o
      case x => JsUndefined("Not tackled : " + me + "+++" + x)
    }
  }

  implicit object JsNullW extends JsValueW[JsNull.type] {
    def +++[T <: JsValue](me:JsNull.type, other: T):JsValue = other match {
      case o if o == JsNull => JsNull
      case x => x
    }
  }

  implicit object JsValueMonoid extends Monoid[JsValue] {
    def append(s1: JsValue, s2: => JsValue) = s1 match {
      case o:JsNull.type => implicitly[JsValueW[JsNull.type]] +++ (o, s2)
      case o:JsUndefined => implicitly[JsValueW[JsUndefined]] +++ (o, s2)
      case o:JsString => implicitly[JsValueW[JsString]] +++ (o, s2)
      case o:JsNumber => implicitly[JsValueW[JsNumber]] +++ (o, s2)
      case o:JsBoolean => implicitly[JsValueW[JsBoolean]] +++ (o, s2)
      case o:JsArray => implicitly[JsValueW[JsArray]] +++ (o, s2)
      case o:JsObject => implicitly[JsValueW[JsObject]] +++ (o, s2)
    }

    val zero = JsNull
  }


  case class JsValueAppendable(js:JsValue) {
    import JsValueMonoid._

    def +++(other:JsValue)(implicit m:Monoid[JsValue]):JsValue = m.append(js, other)
  }

  implicit def jsValueToAppendable(js:JsValue):JsValueAppendable = JsValueAppendable(js)

}

object Node {
  import Neo4JElement._

  implicit object NodeMonoid extends Monoid[Node] {
    def append(s1: Node, s2: => Node) = {
      val newData: JsObject = (s1.data +++ s2.data) match {
        case o: JsObject => o
        case x => throw new IllegalStateException("Cannot add two nodes : " + s1 + " and " + s2)
      }
      val newFields: Seq[(String, JsValue)] = s1.jsValue.fields.filter(_._1 != "data") ++ s2.jsValue.fields.filter(f => f._1 != "data" && s1.jsValue.fields.find(_._1 == f._1).isEmpty)
      val allNewFields: Seq[(String, JsValue)] = ("data", newData) +: newFields
      Node(JsObject(allNewFields))
    }

    val zero = Node(JsObject(Seq("data" -> JsObject(Seq()))))
  }
}
