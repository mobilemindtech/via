# micro-routing
Minimalistic scala routing




```scala

// /test/?ids=123
val test = route(root / "test" /? q_list_int("ids"))

// /test/?id=1&enabled=true
val test = route(root / "test" /? q_int("id") & q_bool("enabled"))

// /test/2024-01-01
val test = route(root / "test" / regex("date", "[0-9]{4,4}-[0-9]{2,2}-[0-9]{2,2}"))

// /test/1/any/thing
val test = route(root / "test" / int("id") / tail("paths"))

// /test/john/show
val test = route(root / "test" / param("name") / "show")

// /test/1/2
val test = route(root / "test" / int("id") / int("id2"))

// process target 
RouteChain.chain(uri, routes) match
  case Ok(route, matcher, params, query, issues) =>
    // found
  case NotFound()

```