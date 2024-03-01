package io.micro.routing

import scala.util.matching.Regex
import io.micro.routing.RegexType.*

import scala.annotation.tailrec
import scala.collection.mutable



trait RequestBuilder[Req <: RouteRequest]:

  def build(method: Method,
            target: String,
            matcher: RouteMatcher,
            params: Params,
            query: Query): Req

trait Router:

  private val _routes = Router.prepare(routes*)

  def routes: Seq[Route]

  val notFound: Route
  
  def dispatch[Req <: RouteRequest, Resp <: RouteResponse](target: String)
                                                          (f: Resp => Unit)
                                                          (using RequestBuilder[Req]): Unit =
    dispatch(Method.All, target)(f)

  def dispatch[Req <: RouteRequest, Resp <: RouteResponse](method: Method,
                                                           target: String)
                                                          (f: Resp => Unit)
                                                          (using RequestBuilder[Req]): Unit =
    doDispatch(method, target, f)

  def dispatchAsync[Req <: RouteRequest, Resp <: RouteResponse](target: String)
                                                               (f: Resp => Unit)
                                                               (using RequestBuilder[Req]): Unit =
    dispatchAsync(Method.All, target)(f)

  def dispatchAsync[Req <: RouteRequest, Resp <: RouteResponse](method: Method,
                                                                target: String)
                                                               (f: Resp => Unit)
                                                               (using RequestBuilder[Req]): Unit =
    doDispatch(method, target, f)

  private def doDispatch[Req <: RouteRequest, Resp <: RouteResponse](method: Method,
                                                                     target: String,
                                                                     f: Resp => Unit)(using builder: RequestBuilder[Req]): Unit =
    RouteChain.chain(method, target, _routes) match
      case i @ Some(info) => doDispatch(
        method, target, info.route, i, f)
      case None =>
        doDispatch(method, target, notFound, None, f)

  private def doDispatch[Req <: RouteRequest, Resp <: RouteResponse](method: Method,
                                                                     target: String,
                                                                     route: Route,
                                                                     routeInfo: Option[RouteInfo],
                                                                     f: Resp => Unit)(using builder: RequestBuilder[Req]): Unit =

    val params = routeInfo.map(_.params).getOrElse(Params())
    val query = routeInfo.map(_.query).getOrElse(Query())
    val matcher = routeInfo.map(_.matcher).getOrElse(Nil)
    val req = builder.build(method, target, matcher, params, query)

    route match
      case rt: RouteController[_] =>
        rt.controller match
          case c: Controller[_, _] =>
            callController(req, c.asInstanceOf[Controller[Req, Resp]], f)
          case c: ControllerAsync[_, _] =>
            callControllerAsync(req, c.asInstanceOf[ControllerAsync[Req, Resp]], f)
          case c: ControllerDispatcher[_, _] =>
            callControllerDispatcher(req, c.asInstanceOf[ControllerDispatcher[Req, Resp]], f)
          case c: ControllerAsyncDispatcher[_, _] =>
            callControllerAsyncDispatcher(req, c.asInstanceOf[ControllerAsyncDispatcher[Req, Resp]], f)
      case rd: RouteDispatcher[_, _] =>
        (rd.dispatch, rd.dispatchAsync) match
          case (Some(d), None) =>
            callRouteDispatcher(req, d.asInstanceOf[RouteCallback[Req, Resp]], f)
          case (None, Some(d)) =>
            callRouteAsyncDispatcher(req, d.asInstanceOf[RouteAsyncCallback[Req, Resp]], f)
          case _ => throw RouteException("wong route dispatcher")
      case _ => throw RouteException("wong dispatcher")
  private def callController[Req <: RouteRequest, Resp <: RouteResponse](req: Req,
                                                                         ctrl: Controller[Req, Resp],
                                                                         f: Resp => Unit): Unit =
    val resp =
      req.method match
        case Method.Get => ctrl.get(req)
        case Method.Post => ctrl.post(req)
        case Method.Put => ctrl.put(req)
        case Method.Delete => ctrl.delete(req)
        case Method.Options => ctrl.options(req)
        case Method.Head => ctrl.head(req)
        case Method.Patch => ctrl.patch(req)
        case _ => throw RouteException("Controller can't handler Method.All")
    resp match
      case Some(r) => f(r)
      case None => throw RouteException(s"Controller does not implements ${req.method}")

  private def callControllerAsync[Req <: RouteRequest, Resp <: RouteResponse](req: Req,
                                                                              ctrl: ControllerAsync[Req, Resp],
                                                                               f: Resp => Unit): Unit =
    req.method match
      case Method.Get => ctrl.get(req, f)
      case Method.Post => ctrl.post(req, f)
      case Method.Put => ctrl.put(req, f)
      case Method.Delete => ctrl.delete(req, f)
      case Method.Options => ctrl.options(req, f)
      case Method.Head => ctrl.head(req, f)
      case Method.Patch => ctrl.patch(req, f)
      case _ => throw RouteException("ControllerAsync can't handler Method.All")

  private def callControllerAsyncDispatcher[Req <: RouteRequest, Resp <: RouteResponse](req: Req,
                                                                                        ctrl: ControllerAsyncDispatcher[Req, Resp],
                                                                                        f: Resp => Unit): Unit =
    ctrl.dispatch(req, f)

  private def callControllerDispatcher[Req <: RouteRequest, Resp <: RouteResponse](req: Req,
                                                                                   ctrl: ControllerDispatcher[Req, Resp],
                                                                                   f: Resp => Unit): Unit =
    f(ctrl.dispatch(req))

  private def callRouteDispatcher[Req <: RouteRequest, Resp <: RouteResponse](req: Req,
                                                                              cb: RouteCallback[Req, Resp],
                                                                              f: Resp => Unit): Unit =
    f(cb(req))

  private def callRouteAsyncDispatcher[Req <: RouteRequest, Resp <: RouteResponse](req: Req,
                                                                                   cb: RouteAsyncCallback[Req, Resp],
                                                                                   f: Resp => Unit): Unit =
    cb(req, f)

type RouteMatcher = List[PathRoot | PathEnd | String | Int | Long]



object Router:

  def mkRouter(notfound: Route, rts: Route*): Router =
    new Router:
      override def routes: Seq[Route] = rts
      override val notFound: Route = notfound

  def routes(route: Route*): Seq[Route] = route

  def route[RouteCtrl <: ControllerBase](path: Path, c: RouteCtrl): Route =
    RouteController(Method.All, path, controller = c)

  def route[TReq <: RouteRequest, TResp <: RouteResponse](path: Path)(f: RouteCallback[TReq, TResp]): Route =
    RouteDispatcher(Method.All, path, dispatch = Some(f))

  def route[TReq <: RouteRequest, TResp <: RouteResponse](path: Path)(f: RouteAsyncCallback[TReq, TResp]): Route =
    RouteDispatcher(Method.All, path, dispatchAsync = Some(f))

  def route[RouteCtrl <: ControllerBase](method: Method, path: Path, c: RouteCtrl): Route =
    RouteController(method, path, controller = c)

  def route[TReq <: RouteRequest, TResp <: RouteResponse](method: Method, path: Path)(f: RouteCallback[TReq, TResp]): Route =
    RouteDispatcher(method, path, dispatch = Some(f))

  def route[TReq <: RouteRequest, TResp <: RouteResponse](method: Method, path: Path)(f: RouteAsyncCallback[TReq, TResp]): Route =
    RouteDispatcher(method, path, dispatchAsync = Some(f))

  def route[RouteCtrl <: ControllerBase](method: Method, route: Route, c: RouteCtrl): Route =
    RouteController(method, route.path, controller = c)

  def route[TReq <: RouteRequest, TResp <: RouteResponse](method: Method, route: Route)(f: RouteCallback[TReq, TResp]): Route =
    RouteDispatcher(method, route.path, dispatch = Some(f))

  def route[TReq <: RouteRequest, TResp <: RouteResponse](method: Method, route: Route)(f: RouteAsyncCallback[TReq, TResp]): Route =
    RouteDispatcher(method, route.path, dispatchAsync = Some(f))

  def middleware[TReq <: RouteRequest, TResp <: RouteResponse](method: Method)(dispatch: MiddlewareDispatcher[TReq, TResp]): Middleware[_, _] =
    Middleware(method, dispatch)

  def middleware[TReq <: RouteRequest, TResp <: RouteResponse](dispatch: MiddlewareDispatcher[TReq, TResp]): Middleware[_, _] =
    Middleware(Method.All, dispatch)

  private def prepare(routes: Route*): Seq[Route] =
    routes.map(_.compile)