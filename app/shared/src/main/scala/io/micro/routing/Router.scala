package io.micro.routing

import scala.util.matching.Regex
import PathParamType.*
import io.micro.routing.RegexType._

import scala.collection.mutable

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

  def process(method: Method, target: String, routes: Seq[Route]) : Option[(Route, List[Param])] =

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

