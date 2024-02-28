package io.micro.routing

import io.micro.routing.Router.*
import io.micro.routing.Path.*
import io.micro.routing.*
import io.micro.routing.Method.*
import org.scalatest.funsuite.AnyFunSuite
import RouteQuery._

type Token = String
type Username = String
type Password = String

case class Auth(token: Token, username: Username)
case class Authenticator(username: Username, password: Password)

case class Request(override val method: Method,
                   override val uri: String,
                   override val route: RoutePath,
                   override val params: Params,
                   override val query: Query,
                   authenticator: Option[Authenticator] = None,
                   auth: Option[Auth] = None) extends RouteRequest(method, uri, route, params, query)


case class Response(body: String = "") extends RouteResponse


class FirstSpec extends AnyFunSuite:



  val token = middleware { (req: Request) => Response() }
  val auth = middleware { (req: Request) => Response() }
  val logger = middleware { (req: Request) => Response() }

  val home = token ++ auth ++ route(Get, root)((req: Request) => Response()) ++ logger

  val user = route(Get, root / "user" ) { (req: Request) => Response() }

  val userSave = route(Post, user) { (req: Request) => Response() }

  val userGet = route(Post, user / int("id")) { (req: Request) => Response() }

  val routes = home :: user :: userSave :: userGet :: Nil

  val notfound = route(any) { (_: Request) => Response("not found")}

  given builder: RequestBuilder[Request] with
    override def build(method: Method, target: String, route: RoutePath, params: Params, query: Query): Request =
      Request(method, target, route, params, query)


  def testPathParams(uri: String, rt: Route, rs: Seq[Route], expecteds: Param*) =

    val router = Router.mkRouter(notfound, rs*)


    router.chain(Method.All, uri) match
      case RouteFound(RouteInfo(route, _, params, _)) =>

        assert(route.pattern == rt.compile.pattern,
          s"wong route found, expect ${rt.compile.pattern} != ${route.pattern}")

        assert(expecteds.size == params.size,
          s"wrong expected params count: ${expecteds.size} != ${params.size}")

        for i <- params.indices do
          val p = params.at(i)
          val exp = expecteds(i)
          assert(p == exp,
            s"wong param name: ${exp.name} != ${p.name}")


      case RouteNotFound() =>
        fail(s"route $uri not found")


  test("route compile"){
    assert(home.compile.pattern.get == "^/$", "wrong compile home")
    assert(user.compile.pattern.get == "^/user$", "wrong compile user")
    assert(userSave.compile.pattern.get == "^/user$", "wrong compile userSave")
    assert(userGet.compile.pattern.get == "^/user/([0-9]+)$", "wrong compile userGet")
  }

  test("route process found param int v1") {

    val user = route(Get,
      root / "user" / int("id")) { (req: Request) => Response() }
    val rt = user.compile
    testPathParams("/user/22", rt, rt :: Nil, ParamInt("id", 22))

  }

  test("route process found param int v2") {

    val user = route(Get,
      root / "user" / int("id")) { (req: Request) => Response() }
    val rt = user.compile

    testPathParams("/user/22", rt, rt :: Nil, ParamInt("id", 22))
  }

  test("route process found param two int") {

    val user = route(Get,
      root / "user" / int("id") / int("id2")) { (req: Request) => Response() }

    assert(user.compile.pattern.get == "^/user/([0-9]+)/([0-9]+)$", "wrong compile user two path params")

    testPathParams("/user/22/100", user,
      user :: Nil, ParamInt("id", 22), ParamInt("id2", 100))

  }

  test("route process found param string") {

    val user = route(Get,
      root / "user" / param("name")) { (req: Request) => Response() }
    
    assert(user.compile.pattern.get == "^/user/(.+)$", "wrong compile user param string")

    testPathParams("/user/john", user,
      user :: Nil, ParamStr("name", "john"))
  }

  test("route process found param string + path") {

    val user = route(Get,
      root / "user" / param("name") / "show") { (req: Request) => Response() }
    
    assert(user.compile.pattern.get == "^/user/(.+)/show$", "wrong compile user param string")

    testPathParams("/user/john/show", user,
      user :: Nil, ParamStr("name", "john"))
  }

  test("route process found param int string") {

    val user = route(Get,
      root / "user" / int("id") / param("name")) { (req: Request) => Response() }
    
    assert(user.compile.pattern.get == "^/user/([0-9]+)/(.+)$", "wrong compile user two path params")

    testPathParams("/user/22/john", user,
      user :: Nil, ParamInt("id", 22), ParamStr("name", "john"))
  }

  test("route process found param tail") {

    val user = route(Get,
      root / "user" / int("id") / tail("paths")) { (req: Request) => Response() }

    val rt = user.compile

    assert(rt.pattern.get == "^/user/([0-9]+)/(.+)$", "wrong compile user two path params")

    testPathParams("/user/22/john/do/ok", rt,
      rt :: Nil, ParamInt("id", 22), ParamPaths("paths", "john" :: "do" :: "ok" :: Nil))
  }

  test("route chain ControllerDispatcher") {

    class MyController extends ControllerDispatcher[Request, Response]:
      override def dispatch(req: Request): Response =
        Response("my controller sync")


    val expected = "my controller sync"
    val userDelete = route(Delete, root / "user", new MyController())

    val router = Router.mkRouter(notfound, userDelete)


    router.dispatch[Request, Response](Method.Delete, "/user") {
      resp =>
        assert(resp.body == expected, "wrong body value")
    }

    router.dispatch[Request, Response](Method.Get, "/user") {
      resp =>
        assert(resp.body == "not found", "wrong body value to not found GET")
    }

    List(Method.Get, Method.Put, Method.Post, Method.Options, Method.Head, Method.Patch).foreach {
      method =>
        router.dispatch[Request, Response](method, "/user") {
          resp =>
            assert(resp.body == "not found", s"wrong body value to not found ${method}")
        }
    }
  }

  test("route chain ControllerAsyncDispatcher") {

    class MyController extends ControllerAsyncDispatcher[Request, Response]:
      override def dispatch(req: Request, f: (resp: Response) => Unit): Unit =
        f(Response("my controller async"))


    val expected = "my controller async"
    val userDelete = route(Delete, root / "user", new MyController())

    val router = Router.mkRouter(notfound, userDelete)

    router.dispatch[Request, Response](Method.Delete, "/user") {
      resp =>
        assert(resp.body == expected, "wrong body value")
    }

    router.dispatch[Request, Response](Method.Get, "/user") {
      resp =>
        assert(resp.body == "not found", "wrong body value to not found GET")
    }

    List(Method.Get, Method.Put, Method.Post, Method.Options, Method.Head, Method.Patch).foreach {
      method =>
        router.dispatch[Request, Response](method, "/user") {
          resp =>
            assert(resp.body == "not found", s"wrong body value to not found ${method}")
        }
    }
  }

  test("route chain RouteCallback") {


    val expected = "my route callback"
    val userDelete = route(Delete, root / "user") {(_: Request) => Response("my route callback")}

    val router = Router.mkRouter(notfound, userDelete)

    router.dispatch[Request, Response](Method.Delete, "/user") {
      resp =>
        assert(resp.body == expected, "wrong body value")
    }

    router.dispatch[Request, Response](Method.Get, "/user") {
      resp =>
        assert(resp.body == "not found", "wrong body value to not found GET")
    }

    List(Method.Get, Method.Put, Method.Post, Method.Options, Method.Head, Method.Patch).foreach {
      method =>
        router.dispatch[Request, Response](method, "/user") {
          resp =>
            assert(resp.body == "not found", s"wrong body value to not found ${method}")
        }
    }
  }

  test("route chain RouteAsyncCallback") {


    val expected = "my route async callback"
    val userDelete = route(Delete, root / "user") { (_: Request, f: (Response) => Unit) => f(Response("my route async callback")) }

    val router = Router.mkRouter(notfound, userDelete)

    router.dispatch[Request, Response](Method.Delete, "/user") {
      resp =>
        assert(resp.body == expected, "wrong body value")
    }

    router.dispatch[Request, Response](Method.Get, "/user") {
      resp =>
        assert(resp.body == "not found", "wrong body value to not found GET")
    }

    List(Method.Get, Method.Put, Method.Post, Method.Options, Method.Head, Method.Patch).foreach {
      method =>
        router.dispatch[Request, Response](method, "/user") {
          resp =>
            assert(resp.body == "not found", s"wrong body value to not found ${method}")
        }
    }
  }

  test("route chain Controller") {

    class MyCtrl extends Controller[RouteRequest, RouteResponse]:
      override def get(req: RouteRequest): Option[RouteResponse] =
        val resp =
          req.route.matcher match
            case root :: "user" :: (id: Int) :: Nil  => Response(s"my route controller show $id")
            case root :: "user" :: Nil => Response("my route controller list")
            case _ => throw Exception("wrong route")
        Some(resp)


    val userShow = route(Get, root / "user" / int("id"), new MyCtrl)
    val userList = route(Get, root / "user", new MyCtrl)

    val router = Router.mkRouter(notfound, userShow, userList)

    router.dispatch[Request, Response](Get, "/user/1") {
      resp =>
        assert(resp.body == "my route controller show 1", "wrong body value")
    }

    router.dispatch[Request, Response](Get, "/user") {
      resp =>
        assert(resp.body == "my route controller list", "wrong body value to not found GET")
    }

    router.dispatch[Request, Response](Post, "/user") {
      resp =>
        assert(resp.body == "not found", "wrong body value to not found POST")
    }
  }

  test("route chain ControllerAsync") {

    class MyCtrl extends ControllerAsync[RouteRequest, RouteResponse]:
      override def get(req: RouteRequest, f: RouteResponse => Unit): Unit =
        val resp =
          req.route.matcher match
            case root :: "user" :: (id: Int) :: Nil => Response(s"my route controller show $id")
            case root :: "user" :: Nil => Response("my route controller list")
            case _ => throw Exception("wrong route")
        f(resp)


    val userShow = route(Get, root / "user" / int("id"), new MyCtrl)
    val userList = route(Get, root / "user", new MyCtrl)

    val router = Router.mkRouter(notfound, userShow, userList)

    router.dispatch[Request, Response](Get, "/user/100") {
      resp =>
        assert(resp.body == "my route controller show 100", "wrong body value")
    }

    router.dispatch[Request, Response](Get, "/user") {
      resp =>
        assert(resp.body == "my route controller list", "wrong body value to not found GET")
    }

    router.dispatch[Request, Response](Post, "/user") {
      resp =>
        assert(resp.body == "not found", "wrong body value to not found POST")
    }
  }

  test("route chain regex") {

    class MyCtrl extends ControllerAsync[RouteRequest, RouteResponse]:
      override def get(req: RouteRequest, f: RouteResponse => Unit): Unit =
        val resp =
          req.route.matcher match
            case root :: "user" :: (date: String) :: Nil => Response(s"route regex ${date}")
            case _ => throw Exception("wrong route")
        f(resp)


    val user = route(Get, root / "user" / regex("date", "[0-9]{4,4}-[0-9]{2,2}-[0-9]{2,2}"), new MyCtrl)

    val router = Router.mkRouter(notfound, user)

    assert(user.compile.pattern.get == "^/user/([0-9]{4,4}-[0-9]{2,2}-[0-9]{2,2})$", "wrong route")

    router.dispatch[Request, Response](Get, "/user/2024-02-01") {
      resp =>
        assert(resp.body == "route regex 2024-02-01", "wrong body value")
    }

    router.dispatch[Request, Response](Get, "/user/01-01-2024") {
      resp =>
        assert(resp.body == "not found", "wrong body value to not found GET")
    }

  }

  test("route chain query int") {
    val user = route(Get, root / "user" /? q_int("id")) {
      (req: Request) =>
        req.query.matcher match
          case  (id: Int) :: Nil => Response(s"query ${id}")
          case _ => throw Exception("wrong route")
    }

    assert(user.compile.pattern.get == "^/user/$", "wrong route")

    val router = Router.mkRouter(notfound, user)

    router.dispatch[Request, Response](Method.Get, "/user/?id=22") {
      resp =>
        assert(resp.body == "query 22", "wrong body value")
    }
  }

  test("route chain query int and bool") {
    val user = route(Get, root / "user" /? q_int("id") & q_bool("enabled")) {
      (req: Request) =>
        req.query.matcher match
          case (id: Int) :: (enabled: Boolean) :: Nil => Response(s"query $id $enabled")
          case _ => throw Exception("wrong route")
    }

    assert(user.compile.pattern.get == "^/user/$", "wrong route")

    val router = Router.mkRouter(notfound, user)

    router.dispatch[Request, Response](Method.Get, "/user/?id=90&enabled=true") {
      resp =>
        assert(resp.body == "query 90 true", "wrong body value")
    }
  }

  test("route chain query int and bool str") {
    val user = route(Get, root / "user" /? q_int("id") & q_bool("enabled") & q_str("name")) {
      (req: Request) =>
        req.query.matcher match
          case (id: Int) :: (enabled: Boolean) :: (name: String) :: Nil =>
            Response(s"query $id $enabled $name")
          case _ => throw Exception("wrong route")
    }

    assert(user.compile.pattern.get == "^/user/$", "wrong route")

    val router = Router.mkRouter(notfound, user)

    router.dispatch[Request, Response](Method.Get, "/user/?id=90&enabled=true&name=ricardo") {
      resp =>
        assert(resp.body == "query 90 true ricardo", "wrong body value")
    }
  }

  test("route chain query list") {
    val user = route(Get, root / "user" /? q_list_int("ids")) {
      (req: Request) =>
        req.query.matcher match
          case (ids: List[Int]) :: Nil =>
            Response(s"query ${ids.mkString(",")}")
          case _ => throw Exception("wrong route")
    }

    assert(user.compile.pattern.get == "^/user/$", "wrong route")

    val router = Router.mkRouter(notfound, user)

    router.dispatch[Request, Response](Method.Get, "/user/?ids=1,2,3,4,5") {
      resp =>
        assert(resp.body == "query 1,2,3,4,5", "wrong body value")
    }
  }

  test("route chain query list option") {
    val user = route(Get, root / "user" /? q_list_int_opt("ids")) {
      (req: Request) =>

        print(s"req.query.matcher = ${req.query.matcher}")

        req.query.matcher match
          case (ids: Option[List[Int]]) :: Nil =>
            Response(s"query ${ids}")
          case _ => throw Exception("wrong route")
    }

    assert(user.compile.pattern.get == "^/user/$", "wrong route")

    val router = Router.mkRouter(notfound, user)

    router.dispatch[Request, Response](Method.Get, "/user/") {
      resp =>
        assert(resp.body == "query None", "wrong body value")
    }
  }