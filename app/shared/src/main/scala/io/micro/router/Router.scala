package io.micro.router

import io.micro.router.core.RouteChain.{RouteFound, RouteNotFound}
import io.micro.router.core.*
import io.micro.router.core.Method

import scala.annotation.tailrec
import scala.reflect.TypeTest

case class RouteInfo(
    method: Method,
    target: String,
    matcher: RouteMatcher,
    params: Params,
    query: Query
)

trait RequestBuilder[Req, Extra]:

  def build(routeInfo: RouteInfo, extra: Option[Extra]): Req

case class Router[Req, Resp, Extra](routes: RouteEntry[Req, Resp]*)(using
    builder: RequestBuilder[Req, Extra],
    ttReq: TypeTest[Any, Req],
    ttResp: TypeTest[Any, Resp]
):

  def dispatch(target: String, extra: Extra): Option[Resp] =
    dispatch(Method.Any, target, extra)

  def dispatch(method: Method, target: String, extra: Extra): Option[Resp] =
    doDispatch(method, target, Some(extra))

  def dispatch(target: String): Option[Resp] =
    dispatch(Method.Any, target)

  def dispatch(
      method: Method,
      target: String
  ): Option[Resp] =
    doDispatch(method, target, None)

  private def doDispatch(
      method: Method,
      target: String,
      extra: Option[Extra]
  ): Option[Resp] =

    val rts = routes.map { entry =>
      entry.route.copy(
        methods = entry.methods,
        tag = Some(entry)
      )
    }
    RouteChain.chain(method, target, rts) match
      case routeFound: RouteFound =>
        doDispatch(method, target, routeFound, extra)
      case RouteNotFound() => None

  private def doDispatch(
      method: Method,
      target: String,
      routeFound: RouteFound,
      extra: Option[Extra] = None
  ): Option[Resp] =

    val route = routeFound.route
    val entry = route.tag.get.asInstanceOf[RouteEntry[Req, Resp]]

    val routeInfo = RouteInfo(
      method,
      target,
      routeFound.matcher,
      routeFound.params,
      routeFound.query
    )
    val req = builder.build(routeInfo, extra)
    val resp =
      entry match
        case r: RouteEntryHandler[Req, Resp] =>
          val resp =
            middlewareBefore(method, req, r.before) match
              case newReq: Req =>
                r.handler.handle(newReq)
              case resp: Resp => resp

          resp |> Some.apply

        case r: RouteEntryDispatcher[Req, Resp] =>
          val resp =
            middlewareBefore(method, req, r.before) match
              case ttReq(newReq) =>
                r.dispatcher(newReq)
              case ttResp(resp) => resp

          resp |> Some.apply

        case r: RouteEntryController[Req, Resp] =>
          middlewareBefore(method, req, r.before) match
            case ttResp(resp) => resp |> Some.apply
            case ttReq(newReq) =>
              val crlResult =
                method match
                  case Method.Get     => r.controller.get(newReq)
                  case Method.Post    => r.controller.post(newReq)
                  case Method.Put     => r.controller.put(newReq)
                  case Method.Delete  => r.controller.delete(newReq)
                  case Method.Options => r.controller.options(newReq)
                  case Method.Head    => r.controller.head(newReq)
                  case Method.Patch   => r.controller.patch(newReq)
                  case Method.Any     => ()
              crlResult match
                case resp: Resp => resp |> Some.apply
                case _: Unit    => // not found
                  // try default handler
                  r.controller.handle(newReq) match
                    case _: Unit    => None // not found
                    case resp: Resp => resp |> Some.apply

    resp match
      case Some(rsp) =>
        middlewareAfter(method, req, rsp, entry.after) |> Some.apply
      case None => None

  @tailrec
  private def middlewareBefore(
      method: Method,
      req: Req,
      beforeOpt: Option[Before[Req, Resp]]
  ): Resp | Req =
    beforeOpt match
      case Some(before) =>
        if before.methods.exists(m => m == Method.Any || m == method)
        then
          before.handler(req) match
            case ttReq(newReq) =>
              middlewareBefore(method, newReq, before.next)
            case ttResp(resp) =>
              resp
        else middlewareBefore(method, req, before.next)
      case _ => req

  @tailrec
  private def middlewareAfter(
      method: Method,
      req: Req,
      resp: Resp,
      afterOpt: Option[After[Req, Resp]]
  ): Resp =
    afterOpt match
      case Some(after) =>
        val newResp =
          if after.methods.exists(m => m == Method.Any || m == method)
          then after.handler(req, resp)
          else resp
        middlewareAfter(method, req, newResp, after.next)
      case _ => resp

object Router:

  def verbs(methods: Method*): Seq[Method] = methods

  def route[Req, Resp](
      path: Path,
      c: Controller[Req, Resp]
  ): RouteEntry[Req, Resp] =
    route(Method.Any, path, c)

  def route[Req, Resp](
      method: Method,
      path: Path,
      c: Controller[Req, Resp]
  ): RouteEntry[Req, Resp] =
    route(method :: Nil, path, c)

  def route[Req, Resp](
      methods: Seq[Method],
      path: Path,
      c: Controller[Req, Resp]
  ): RouteEntry[Req, Resp] =
    RouteEntryController(methods, Route.route(path), controller = c)

  def route[Req, Resp](path: Path)(
      c: Handler[Req, Resp]
  ): RouteEntry[Req, Resp] =
    route(Method.Any, path)(c)

  def route[Req, Resp](method: Method, path: Path)(
      c: Handler[Req, Resp]
  ): RouteEntry[Req, Resp] =
    route(method :: Nil, path)(c)

  def route[Req, Resp](methods: Seq[Method], path: Path)(
      f: Handler[Req, Resp]
  ): RouteEntry[Req, Resp] =
    RouteEntryHandler(methods, Route.route(path), handler = f)

  def route[Req, Resp](path: Path)(
      f: Dispatcher[Req, Resp]
  ): RouteEntry[Req, Resp] =
    route(Method.Any, path)(f)

  def route[Req, Resp](method: Method, path: Path)(
      f: Dispatcher[Req, Resp]
  ): RouteEntry[Req, Resp] =
    route(method :: Nil, path)(f)

  def route[Req, Resp](methods: Seq[Method], path: Path)(
      f: Dispatcher[Req, Resp]
  ): RouteEntry[Req, Resp] =
    RouteEntryDispatcher(methods, Route.route(path), dispatcher = f)

  def route[Req, Resp](
      methods: Seq[Method],
      r: Route,
      c: Controller[Req, Resp]
  ): RouteEntry[Req, Resp] =
    RouteEntryController(methods, r, controller = c)

  def route[Req, Resp](
      methods: Seq[Method],
      r: Route,
      c: Handler[Req, Resp]
  ): RouteEntry[Req, Resp] =
    RouteEntryHandler(methods, r, handler = c)

  def route[Req, Resp](methods: Seq[Method], r: Route)(
      f: Dispatcher[Req, Resp]
  ): RouteEntry[Req, Resp] =
    RouteEntryDispatcher(methods, r, dispatcher = f)

  def after[Req, Resp](methods: Method*)(
      dispatch: MiddlewareAfter[Req, Resp]
  ): After[Req, Resp] =
    After(methods, dispatch)

  def after[Req, Resp](dispatch: MiddlewareAfter[Req, Resp]): After[Req, Resp] =
    After(Method.Any :: Nil, dispatch)

  def before[Req, Resp](methods: Method*)(
      dispatch: MiddlewareBefore[Req, Resp]
  ): Before[Req, Resp] =
    Before(methods, dispatch)

  def before[Req, Resp](
      dispatch: MiddlewareBefore[Req, Resp]
  ): Before[Req, Resp] =
    Before(Method.Any :: Nil, dispatch)
