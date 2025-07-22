# via
Minimalistic http routing dispatcher for Scala

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

// chain target 
RouteChain.chain(uri, Seq(routeA, routeB)) match
  case Ok(route, matcher, params, query, issues) =>
    // found
  case NotFound()

```