package be.nextlab.play.neo4j.rest

import org.specs2.Specification
import play.api.libs.concurrent.Promise
import play.api.test._
import play.api.test.Helpers._
import Neo4JTestHelpers._

/**
 *
 * User: noootsab
 * Date: 1/04/12
 * Time: 14:34
 */

object RootTest extends Specification {

  def is = "Test service root" ^ {

    "Get it" ! neoApp {
      val root: Promise[Root] = endPoint.root
      await (root) must be like {
        case r => r.neo4jVersion must be_=== (plugin.neo4jVersion)
      }
    }

  }

}
