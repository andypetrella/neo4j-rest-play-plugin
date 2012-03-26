package be.nextlab.play.neo4j.rest

import play.api.libs.json._
import play.api.Play._
import dispatch._
import play.api.Play
import be.nextlab.play.neo4j.rest.util.PlayJsonDispatch._
import play.api.libs.json.Json._

/**
 * User: andy
 */

class Neo4JService {
  lazy val neo4jPlugin = Play.maybeApplication map {
    app => {
      app.plugin[Neo4JRestPlugin] match {
        case Some(p) => {
          if (p.enabled()) {
            p
          } else {
            throw throw new IllegalStateException("This Plugin is not enabled in this application")
          }
        }
        case None => throw new IllegalStateException("This Plugin is not defined in this application")
      }
    }
  } getOrElse (throw new IllegalStateException("The application hasn't yet started!"))


  lazy val neoRestBase = neo4jPlugin.baseRestEndPoint / "db" / "data"
  lazy val neoRestNode = neoRestBase / "node"
  lazy val neoRestRel = neoRestBase / "relationship"
  lazy val neoRestCypher = neoRestBase / "cypher"

  def neoRestNodeIndex(indexName: String) = neoRestBase / "index" / "node" / indexName

  def neoRestNodeById(id: Int) = neoRestNode / id.toString

  def neoRestRelById(id: Int) = neoRestRel / id.toString

  def buildUrl(u: String) = neo4jPlugin.createRequest(Left(u))

  lazy val root: Neo4JElement = Http(neoRestBase <:< Map("Accept" -> "application/json") |>! {
    {case j:JsObject => Root(j)}
  })

  def getNode(id: Int): Neo4JElement = {
    Http((neoRestNodeById(id) <:< Map("Accept" -> "application/json") |>! ({case j:JsObject => Node(j)})) >! {
      case e => {
        //todo find the type of the exception thrown for a 404 => node not existing

        //todo remove this...
        e.printStackTrace()
        //todo how to retrieve the error message (json)...
        Failure(
          JsObject(Seq(
            "message" -> JsString(e.getMessage),
            "exception" -> JsString(e.toString),
            "stacktrace" -> JsArray())
          )
        )
      }
    })
  }

  def createNode: Neo4JElement = Http(
    (neoRestNode POST)
      <:< Map("Accept" -> "application/json")
      |>! {
      {case j:JsObject => Node(j)}
    }
  )

  def createNode(properties: JsObject): Neo4JElement = Http(
    (neoRestNode <<(stringify(properties), "application/json"))
      <:< Map("Accept" -> "application/json")
      |>! {
      {case j:JsObject => Node(j)}
    }
  )

  def properties(node:Node, props:Option[JsObject]):Neo4JElement = {
    Node(JsObject(Seq()))
    /*todo props match {
      case None => Http(
          (buildUrl(node.properties))
            <:< Map("Accept" -> "application/json")
            |>! {
            {case j:JsObject => node.copy(node.jsValue ++ JsObject(Seq("data" -> j)))}
          }
        )
      case Some(p) => {
        case JsObject(x :: Nil) =>  Http(
          (buildUrl(node.property replace ("{key}", x._1)) << (stringify(x._2), "application/json") )
            <:< Map("Accept" -> "application/json")
            |>! {
            {case j:JsObject => node.copy(node.jsValue ++ JsObject(Seq("data" -> j)))}
          }
        )
        case o => Http(
          (buildUrl(node.properties) << (stringify(o), "application/json") )
            <:< Map("Accept" -> "application/json")
            |>! {
            {case j:JsObject => node.copy(node.jsValue ++ JsObject(Seq("data" -> j)))}
          }
        )
      }
    }*/
  }


  def deleteNode(id: Int): Neo4JElement = Http((((neoRestNodeById(id) DELETE) >|) ~> {u => Empty()}) >! {
    case e => {
      //todo find the type of the exception thrown for a 404 => node not existing

      //todo remove this...
      e.printStackTrace()
      //todo how to retrieve the error message (json)...
      Failure(
        JsObject(Seq(
          "message" -> JsString(e.getMessage),
          "exception" -> JsString(e.toString),
          "stacktrace" -> JsArray())
        )
      )
    }
  })


}
