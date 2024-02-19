# micro-routing
Minimalistic scala routing




```scala

case class Request(override val uri: String,
                   override val method: String,
                   override val path: Path) extends RouteRequest(uri, method, path)

case class Response(override val params: List[Param] = Nil) extends RouteResponse(params)

class MyController extends ControllerSimple:

type Req = Request
type Resp = Response

override def dispatch(req: Req): Resp = ???




val token = middleware { (req: Request) => Response() }
val auth = middleware { (req: Request) => Response() }
val logger = middleware { (req: Request) => Response() }

val home = token ++ auth ++ route(Get, root)((req: Request) => Response()) ++ logger

val user = route(Get, root / "user") { (req: Request) => Response() }

val userSave = route(Post, user) { (req: Request) => Response() }

val userGet = route(Post, user / int("id")) { (req: Request) => Response() }

val userDelete = route(Delete, user, new MyController())

val routes = prepare(home, user, userSave, userGet, userDelete)

process(Get, "/user/10", routes) match
  case Some((r, params)) =>
    params match
      case PathInt(_, id) :: _ =>
        println(s"route found, id = $id")
       
  case None => println("route not found")

```