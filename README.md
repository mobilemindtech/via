# via
Minimalistic http routing dispatcher for Scala

## Features

* Enter and leave middleware
* Route params
* Route query

## Routes examples

Complete code at [https://github.com/mobilemindtech/via/blob/master/via/jvm/src/test/scala/io/via/RoutingTest.scala]

```scala

import via.*

// /test/?ids=123
route(root / "test" /? q_list_int("ids")) { req => ??? }

// /test/?id=1&enabled=true
route(root / "test" /? q_int("id") & q_bool("enabled")) { req => ??? }

// /test/2024-01-01
route(root / "test" / regex("date", "[0-9]{4,4}-[0-9]{2,2}-[0-9]{2,2}")) { req => ??? }

// /test/1/any/thing
route(root / "test" / int("id") / tail("paths")) { req => ??? }

// /test/john/show
route(root / "test" / param("name") / "show") { req => ??? }

// /test/1/2
route(root / "test" / int("id") / int("id2")) { req => ??? }

// /
route(Get, "/")

// /user/1 => req.params.str("id") == Some("1")
route(Get, "/user/:id") { req => ??? }

// /user/1 => req.params.int("id") == Some(1)
route(Get, "/user/:id(int)") { req => ??? }

// /user/a/b => req.params.tail("paths") == List("a", "b")
route(Get, "/user/*") { req => ??? }

// apply route chain
RouteChain.chain(uri, Seq(routeA, routeB)) match
  case Ok(route, matcher, params, query, issues) =>
    // found
  case NotFound()

```

## Middleware example

Complete code at [https://github.com/mobilemindtech/via/blob/master/via/jvm/src/test/scala/io/via/RouterTest.scala]

```scala
import via.*

val index = route(GET, root) { (req: Request) =>
    Response(200, s"${req.body.get} ${req.auth.get.username}", "text/plain")
}

val auth = enter { (req: Request) =>
    req.headers.get("Authorization") match
        case Some(token) =>
            users.get(token) match
                case Some(username) =>
                    val auth = Auth(username, token) |> Some.apply
                    req.copy(auth = auth)
                case _ => Response.unauthorized
        case _ => Response.unauthorized
}

val validation = enter { (req: Request) =>
    req.body match
        case None => Response.badRequest
        case _    => req
}

val authIndex = auth ++ validation ++ index

val router = Router[Request, ResponseText, RequestExtra](authIndex)

router.dispatch(GET, "/") match
    case Some(resp) =>
        assert(resp.body.contains("hello jonh@gmail.com"), "expected response hello jonh@gmail.com")
    case None => fail("resp can't be none")

router.dispatch(GET, "/", extra.copy(body = None)) match
    case Some(resp) =>
        assert(resp.status == 400,"expected response status 400")
    case None => fail("resp can't be none")

```