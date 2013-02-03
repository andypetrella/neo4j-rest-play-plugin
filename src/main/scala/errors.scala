package play.neo4j.rest

import play.api.libs.json.JsValue
import play.api.libs.ws.Response


case class BadJson[T](js:JsValue, f:PartialFunction[JsValue, T]) extends Exception
case class BadStatus[T](resp:Response, f:PartialFunction[Response, T]) extends Exception