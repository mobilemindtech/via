package io.micro.routing

import io.micro.routing.Path.*
import io.micro.routing.router.Router.{before, route}
import io.micro.routing.router.{
  Method,
  RequestBuilder,
  RouteEntry,
  RouteInfo,
  Router
}
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.Future

type Headers = Map[String, String]

case class Auth(username: String, token: String)

case class Request(
    method: Method,
    target: String,
    params: Params,
    query: Query,
    matcher: RouteMatcher,
    body: Option[String] = None,
    headers: Headers = Map(),
    auth: Option[Auth] = None
)

case class RequestExtra(body: Option[String] = None, headers: Headers = Map())

case class Response(
    status: Int,
    body: Option[String] = None,
    contentType: Option[String] = None
)

object Response:
  def notFound: Response = Response(404)
  def serverError: Response = Response(500)
  def badRequest: Response = Response(400)

  def unauthorized: Response = Response(401)
  def ok: Response = Response(200)

  def apply(status: Int, body: String): Response =
    Response(status, Some(body), None)

  def apply(status: Int, body: String, contentType: String): Response =
    Response(status, Some(body), Some(contentType))

given RequestBuilder[Request, RequestExtra] with
  override def build(
      routeInfo: RouteInfo,
      extra: Option[RequestExtra]
  ): Request =
    Request(
      routeInfo.method,
      routeInfo.target,
      routeInfo.params,
      routeInfo.query,
      routeInfo.matcher,
      body = extra.flatMap(_.body),
      headers = extra.map(_.headers).getOrElse(Map())
    )

// sbt testOnly *RouterTest
class RouterTest extends AnyFunSuite:

  val users = Map(
    "123456" -> "jonh@gmail.com"
  )

  test("router GET /") {

    val entry = route(Method.Get, root) { (req: Request) =>
      Response(200, "OK", "text/plain")
    }

    val router = Router[Request, Response, RequestExtra](entry)

    router.dispatch(Method.Get, "/", None) match
      case Some(resp) =>
        assert(resp.body.contains("OK"), "expected response OK")
      case None => fail("resp can't be none")
  }

  test("router GET  with middleware") {

    val index = route(Method.Get, root) { (req: Request) =>
      Response(200, s"hello ${req.auth.get.username}", "text/plain")
    }

    val auth = before { (req: Request) =>
      req.headers.get("Authorization") match
        case Some(token) =>
          users.get(token) match
            case Some(username) =>
              val auth = Auth(username, token) |> Some.apply
              req.copy(auth = auth)
            case _ => Response.unauthorized
        case _ => Response.unauthorized
    }

    val authIndex = auth ++ index

    val router = Router[Request, Response, RequestExtra](authIndex)
    val extra = RequestExtra(headers = Map("Authorization" -> "123456"))
    router.dispatch(Method.Get, "/", Some(extra)) match
      case Some(resp) =>
        assert(
          resp.body.contains("hello jonh@gmail.com"),
          "expected response hello jonh@gmail.com"
        )
      case None => fail("resp can't be none")

    router.dispatch(Method.Get, "/", None) match
      case Some(resp) =>
        assert(
          resp.status == 401,
          "expected response status 401"
        )
      case None => fail("resp can't be none")

  }
