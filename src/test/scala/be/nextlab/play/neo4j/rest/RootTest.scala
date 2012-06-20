package be.nextlab.play.neo4j.rest

import org.specs2.Specification
import play.api.libs.concurrent.Promise
import play.api.test._
import play.api.test.Helpers._
import Neo4JTestHelpers._
import scalaz.{Success => OK, Failure => KO, _}
import scalaz.Scalaz._
import ValidationPromised._ 
import Neo4JElement._ 

/**
 *
 * User: noootsab
 * Date: 1/04/12
 * Time: 14:34
 */

object RootTest extends Specification {

  def is = "Test service root" ^ {

    "Get it" ! neoApp {
      await (endPoint.root) must be like {
        case OK(r) => r.neo4jVersion must be_=== (plugin.neo4jVersion)
      }
    } ^ 
    "Dummy test for >>*<< " ! {
        val r1r2 = endPoint.root.map(x => Seq(x)).await.get >>*<< endPoint.root.map(x => Seq(x)).await.get

        r1r2.toOption.get must be like {
            case Seq(r1, r2) => ok("good")
            case _ => ko("not good")
        }
    }
  }

}
