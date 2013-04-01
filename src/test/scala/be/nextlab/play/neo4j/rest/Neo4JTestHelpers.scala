package be.nextlab.play.neo4j.rest

import play.api.test._
import play.api.test.Helpers._
import org.specs2.execute.Result
import org.specs2.specification.Around

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

/**
 *
 * User: noootsab
 * Date: 1/04/12
 * Time: 14:30
 */

object Neo4JTestHelpers {
  val fakeApp = FakeApplication(additionalPlugins = Seq("be.nextlab.play.neo4j.rest.Neo4JRestPlugin"))

  lazy val plugin: Neo4JRestPlugin = fakeApp.plugin[Neo4JRestPlugin].get
  lazy implicit val endPoint: Neo4JEndPoint = plugin.neo4j

  object neoApp extends Around {
    def around[T <% Result](t: => T) = {
      val app: FakeApplication = fakeApp
      running(app) {
        val result = t
        cleaned
        result
      }
    }
    def cleaned(implicit ex:ExecutionContext) =
      await((for {
        r <- endPoint.root
        ref <- r.referenceNode
        //l <- r.cypher(Cypher("start r=relationship(*) delete r"))
        n <- r.cypher(Cypher(s"start n=node(*) match n-[r?]-() where not(id(n) = ${ref.id}) delete n,r"))
      } yield ()).map{ _ => println("database has been cleaned")}.recover {
        case x => println("Failed to clean the database : " + x)
      })

  }

}
