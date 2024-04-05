package io.micro.routing.router

import io.micro.routing.RouteChain.{RouteFound, RouteNotFound}
import io.micro.routing.*

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

  def dispatch(target: String, extra: Option[Extra]): Option[Resp] =
    dispatch(Method.Any, target, extra)

  def dispatch(
      method: Method,
      target: String,
      extra: Option[Extra]
  ): Option[Resp] =
    doDispatch(method, target, extra)

  private def doDispatch(
      method: Method,
      target: String,
      extra: Option[Extra]
  ): Option[Resp] =

    val rts = routes.map(_.route)
    RouteChain.chain(target, rts) match
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
    val entry = routes.find(_.route == route).get

    val methodAllow =
      entry.method == Method.Any || method == Method.Any || entry.method == method

    if !methodAllow
    then None
    else
      val routeInfo = RouteInfo(
        method,
        target,
        routeFound.matcher,
        routeFound.params,
        routeFound.query
      )
      val req = builder.build(routeInfo, extra)
      entry match
        case r: RouteEntryHandler[Req, Resp] =>
          val resp =
            middlewareBefore(req, r.before) match
              case newReq: Req =>
                r.handler.handle(newReq)
              case resp: Resp => resp

          middlewareAfter(resp, r.after) |> Some.apply

        case r: RouteEntryDispatcher[Req, Resp] =>
          val resp =
            middlewareBefore(req, r.before) match
              case ttReq(newReq) =>
                r.dispatcher(newReq)
              case ttResp(resp) => resp

          middlewareAfter(resp, r.after) |> Some.apply

        case r: RouteEntryController[Req, Resp] =>
          val resp =
            middlewareBefore(req, r.before) match
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
              middlewareAfter(rsp, r.after) |> Some.apply
            case _ => None // not found

  @tailrec
  private def middlewareBefore(
      req: Req,
      beforeOpt: Option[Before[Req, Resp]]
  ): Resp | Req =
    beforeOpt match
      case Some(before) =>
        before.handler(req) match
          case ttReq(newReq) =>
            middlewareBefore(newReq, before.next)
          case ttResp(resp) =>
            resp
      case _ => req

  private def middlewareAfter(
      resp: Resp,
      afterOpt: Option[After[Req, Resp]]
  ): Resp =
    afterOpt match
      case Some(after) =>
        after.handler(resp)
      case _ => resp

object Router:

  def route[Req, Resp](
      path: Path,
      c: Controller[Req, Resp]
  ): RouteEntry[Req, Resp] =
    route(Method.Any, path, c)

  def route[Req, Resp](path: Path)(
      f: Dispatcher[Req, Resp]
  ): RouteEntry[Req, Resp] =
    route(Method.Any, path)(f)

  def route[Req, Resp](path: Path)(
      c: Handler[Req, Resp]
  ): RouteEntry[Req, Resp] =
    route(Method.Any, path)(c)

  def route[Req, Resp](
      method: Method,
      path: Path,
      c: Controller[Req, Resp]
  ): RouteEntry[Req, Resp] =
    RouteEntryController(method, Route.route(path), controller = c)

  def route[Req, Resp](method: Method, path: Path)(
      f: Handler[Req, Resp]
  ): RouteEntry[Req, Resp] =
    RouteEntryHandler(method, Route.route(path), handler = f)

  def route[Req, Resp](method: Method, path: Path)(
      f: Dispatcher[Req, Resp]
  ): RouteEntry[Req, Resp] =
    RouteEntryDispatcher(method, Route.route(path), dispatcher = f)

  def route[Req, Resp](
      method: Method,
      r: Route,
      c: Controller[Req, Resp]
  ): RouteEntry[Req, Resp] =
    RouteEntryController(method, r, controller = c)

  def route[Req, Resp](
      method: Method,
      r: Route,
      c: Handler[Req, Resp]
  ): RouteEntry[Req, Resp] =
    RouteEntryHandler(method, r, handler = c)

  def route[Req, Resp](method: Method, r: Route)(
      f: Dispatcher[Req, Resp]
  ): RouteEntry[Req, Resp] =
    RouteEntryDispatcher(method, r, dispatcher = f)

  def after[Req, Resp](method: Method)(
      dispatch: MiddlewareAfter[Req, Resp]
  ): After[Req, Resp] =
    After(method, dispatch)

  def after[Req, Resp](dispatch: MiddlewareAfter[Req, Resp]): After[Req, Resp] =
    After(Method.Any, dispatch)

  def before[Req, Resp](method: Method)(
      dispatch: MiddlewareBefore[Req, Resp]
  ): Before[Req, Resp] =
    Before(method, dispatch)

  def before[Req, Resp](
      dispatch: MiddlewareBefore[Req, Resp]
  ): Before[Req, Resp] =
    Before(Method.Any, dispatch)
