package io.micro.routing

import scala.util.matching.Regex
import PathParamType.*
import io.micro.routing.RegexType._

import scala.collection.mutable

case class RouteException(message: String) extends Exception(message)

sealed trait RouteMatch
case class RouteFound(route: Route, params: Seq[Param]) extends RouteMatch
case class RouteNotFound() extends RouteMatch

trait RequestBuilder[Req <: RouteRequest]:

  def build(method: Method, target: String, path: Path, params: Seq[Param]): Req

trait Router(val routes: Seq[Route]):

  def notfound: Option[Route] = None

  def chain(target: String): RouteMatch =
    chain(Method.All, target)
  def chain(method: Method, target: String): RouteMatch =
    Router.chain(method, target, routes) match
      case Some((route, params)) => RouteFound(route, params)
      case None => RouteNotFound()

  def dispatch[Req <: RouteRequest, Resp <: RouteResponse](target: String)(f: Unit => Resp)(using RequestBuilder[Req]): Unit =
    dispatch(Method.All, target)(f)

  def dispatch[Req <: RouteRequest, Resp <: RouteResponse](method: Method, target: String)(f: Unit => Resp)(using RequestBuilder[Req]): Unit =
    dispatch(method, target, Some(f), None)

  def dispatchAsync[Req <: RouteRequest, Resp <: RouteResponse](target: String)(f: Resp => Unit)(using RequestBuilder[Req]): Unit =
    dispatchAsync(Method.All, target)(f)

  def dispatchAsync[Req <: RouteRequest, Resp <: RouteResponse](method: Method, target: String)(f: Resp => Unit)(using RequestBuilder[Req]): Unit =
    dispatch(method, target, None, Some(f))

  private def dispatch[Req <: RouteRequest, Resp <: RouteResponse](method: Method,
               target: String,
               sync: Option[Unit => Resp],
               async: Option[Resp => Unit])(using RequestBuilder[Req]): Unit =
    Router.chain(method, target, routes) match
      case Some((route, params)) => dispatch(method, target, route, params, sync, async)
      case None =>
        notfound match
          case Some(route) => dispatch(method, target, route, Nil, sync, async)
          case _ => throw RouteException("route not found")
         
  private def dispatch[Req <: RouteRequest, Resp <: RouteResponse](method: Method,
               target: String,
               route: Route,
               params: Seq[Param],
               sync: Option[Unit => Resp],
               async: Option[Resp => Unit])(using builder: RequestBuilder[Req]): Unit =
    val req = builder.build(method, target, route.path, params)
    dispatcher(method, route) match
      case routeSync: RouteCallback[_, _] =>
        val routeSync_f = routeSync.asInstanceOf[RouteCallback[Req, Resp]]
        sync match
          case Some(f) =>
            f(routeSync_f(req))
          case None => throw RouteException("sync callback not found")
      case routeAsync: RouteAsyncCallback[_, _] =>
        val routeAsync_f = routeAsync.asInstanceOf[RouteAsyncCallback[Req, Resp]]
        async match
          case Some(f) =>
            routeAsync_f(req, f)
          case None => throw RouteException("async callback not found")


  def dispatcher[Req <: RouteRequest, Resp <: RouteResponse](method: Method, route: Route): RouterDispatcher[Req, Resp] =
    route match
      case rt: RouteController[_] =>
        rt.controller match
          case c: Controller[_, _] =>
            val f =
              method match
                case Method.Get => c.get
                case Method.Post => c.post
                case Method.Put => c.put
                case Method.Delete => c.delete
                case Method.Options => c.options
                case Method.Head => c.head
                case Method.Patch => c.patch
                case _ => throw RouteException("Controller can't handler Method.All")
            f.asInstanceOf[RouteCallback[Req, Resp]]
          case c: ControllerAsync[_, _] =>
            val f =
              method match
                case Method.Get => c.get
                case Method.Post => c.post
                case Method.Put => c.put
                case Method.Delete => c.delete
                case Method.Options => c.options
                case Method.Head => c.head
                case Method.Patch => c.patch
                case _ => throw RouteException("ControllerAsync can't handler Method.All")
            f.asInstanceOf[RouteAsyncCallback[Req, Resp]]
          case c: ControllerDispatcher[_, _] =>
            c.dispatch.asInstanceOf[RouteCallback[Req, Resp]]
          case c: ControllerAsyncDispatcher[_, _] =>
            c.dispatch.asInstanceOf[RouteAsyncCallback[Req, Resp]]
      case rd: RouteDispatcher[_, _] =>
        rd.dispatch.asInstanceOf[RouterDispatcher[Req, Resp]]
      case _ => throw RouteException("wong dispatcher")


object Router:

  def routes(route: Route*): Seq[Route] = route

  def route[RouteCtrl <: ControllerBase](path: Path, c: RouteCtrl): Route =
    RouteController(Method.All, path, controller = c)

  def route[TReq <: RouteRequest, TResp <: RouteResponse](path: Path)(f: RouteCallback[TReq, TResp]): Route =
    RouteDispatcher(Method.All, path, dispatch = f)

  def route[TReq <: RouteRequest, TResp <: RouteResponse](path: Path)(f: RouteAsyncCallback[TReq, TResp]): Route =
    RouteDispatcher(Method.All, path, dispatch = f)

  def route[RouteCtrl <: ControllerBase](method: Method, path: Path, c: RouteCtrl): Route =
    RouteController(method, path, controller = c)

  def route[TReq <: RouteRequest, TResp <: RouteResponse](method: Method, path: Path)(f: RouteCallback[TReq, TResp]): Route =
    RouteDispatcher(method, path, dispatch = f)

  def route[TReq <: RouteRequest, TResp <: RouteResponse](method: Method, path: Path)(f: RouteAsyncCallback[TReq, TResp]): Route =
    RouteDispatcher(method, path, dispatch = f)

  def route[RouteCtrl <: ControllerBase](method: Method, route: Route, c: RouteCtrl): Route =
    RouteController(method, route.path, controller = c)

  def route[TReq <: RouteRequest, TResp <: RouteResponse](method: Method, route: Route)(f: RouteCallback[TReq, TResp]): Route =
    RouteDispatcher(method, route.path, dispatch = f)

  def route[TReq <: RouteRequest, TResp <: RouteResponse](method: Method, route: Route)(f: RouteAsyncCallback[TReq, TResp]): Route =
    RouteDispatcher(method, route.path, dispatch = f)

  infix def / (route: Route, path: Path): Path =
    route.path / path

  infix def / (route: Route, path: String): Path =
    route.path / path

  infix def int(name: String): Path =
    regex(regexint(name))

  infix def long(name: String): Path =
    regex(regexlong(name))

  infix def param(name: String): Path =
    regex(regexany(name))

  infix def tail(name: String): Path =
    paths(name)

  infix def paths(name: String): Path =
    regex(regexpaths(name))

  infix def regex(regex: PathParamRegex): Path =
    Path(PathParam(regex.name, regex))

  def middleware[TReq <: RouteRequest, TResp <: RouteResponse](method: Method)(dispatch: MiddlewareDispatcher[TReq, TResp]): Middleware[_, _] =
    Middleware(method, dispatch)

  def middleware[TReq <: RouteRequest, TResp <: RouteResponse](dispatch: MiddlewareDispatcher[TReq, TResp]): Middleware[_, _] =
    Middleware(Method.All, dispatch)

  def prepare(routes: Route*): Seq[Route] =
    routes.map(_.compile())

  def chain(method: Method, target: String, routes: Seq[Route]) : Option[(Route, List[Param])] =

    def find(rts: List[Route]): Option[(Route, List[Param])] =
      rts match
        case x :: xs =>
          val pattern = x.pattern.get
          val regex = new Regex(pattern)
          val results = regex.findAllIn(target)

          val methodMatch =
            method == x.method || method == Method.All || x.method == Method.All

          if !methodMatch || results.isEmpty
          then
            //println(s">> not match $target = $pattern, method = $methodMatch")
            find(xs)
          else
            x.params match
              case None | Some(Nil) => Some(x, Nil)
              case Some(pathParams) =>

                val params = mutable.ListBuffer[Param]()

                for i <- 1 to results.groupCount do
                  val paramVal = results.group(i)
                  val param = pathParams(i-1)
                  param.regex.typ match
                    case RegexStr =>
                      params.append(ParamStr(param.name, paramVal))
                    case RegexInt =>
                      params.append(ParamInt(param.name, paramVal.toInt))
                    case RegexLong =>
                      params.append(ParamLong(param.name, paramVal.toLong))
                    case RegexPaths =>
                      params.append(ParamPaths(param.name, paramVal.split("/").toList))

                Some(x, params.toList)

        case _ => None

    find(routes.toList)

