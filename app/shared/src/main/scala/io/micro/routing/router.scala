package io.micro.routing

import scala.util.matching.Regex
import io.micro.routing.RegexType.*
import io.micro.routing.QueryType._

import scala.collection.mutable


case class RouteException(message: String) extends Exception(message)

sealed trait RouteState
case class RouteFound(info: RouteInfo) extends RouteState
case class RouteNotFound() extends RouteState

trait RequestBuilder[Req <: RouteRequest]:

  def build(method: Method, target: String, route: RoutePath, params: Params, query: Query): Req

trait Router:

  private val _routes = Router.prepare(routes*)

  def routes: Seq[Route]

  val notFound: Route

  def chain(target: String): RouteState =
    chain(Method.All, target)
  def chain(method: Method, target: String): RouteState =
    Router.chain(method, target, _routes) match
      case Some(info) => RouteFound(info)
      case None => RouteNotFound()

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
    Router.chain(method, target, _routes) match
      case Some(info) => doDispatch(
        method, target, info.route, info.routePath, info.params, info.query, f)
      case None =>
        doDispatch(method, target, notFound, RoutePath(), Params(), Query(), f)

  private def doDispatch[Req <: RouteRequest, Resp <: RouteResponse](method: Method,
                                                                     target: String,
                                                                     route: Route,
                                                                     routePath: RoutePath,
                                                                     params: Params,
                                                                     query: Query,
                                                                     f: Resp => Unit)(using builder: RequestBuilder[Req]): Unit =

    val req = builder.build(method, target, routePath, params, query)

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


enum QueryType:
  case QueryStr(v: String) extends QueryType
  case QueryInt(v: Int) extends QueryType
  case QueryLong(v: Long) extends QueryType
  case QueryBool(v: Boolean) extends QueryType
  case QueryList(v: QueryListType)  extends QueryType
  case QueryOption(v: QueryOptType)  extends QueryType
  case QueryInvalid(msg: String) extends QueryType

type QueryListType = List[QueryStr | QueryInt | QueryLong | QueryInvalid]

type QueryOptType = Option[QueryStr | QueryInt | QueryLong | QueryBool | QueryList]

case class QueryParam(name: String, value: QueryType)

type QueryMatcherListType = List[Int | Long | String]
type QueryMatcherOptType = Option[Int | Long | String | Boolean | QueryMatcherListType]
type QueryMatcherType = Int | Long | String | Boolean | QueryMatcherOptType | QueryMatcherListType

type QueryMatcher = List[QueryMatcherType]

case class RouteInfo(route: Route,
                     routePath: RoutePath,
                     params: Params,
                     query: Query)

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

  def prepare(routes: Route*): Seq[Route] =
    routes.map(_.compile)

  def chain(method: Method, target: String, routes: Seq[Route]) : Option[RouteInfo] =

    val uri =
      if target.contains("?")
      then target.split('?').head
      else target

    def find(rts: List[Route]): Option[RouteInfo] =
      rts match
        case x :: xs =>
          val pattern = x.pattern.get
          val regex = new Regex(pattern)
          val results = regex.findAllIn(uri)

          val methodMatch =
            method == x.method || method == Method.All || x.method == Method.All

          if !methodMatch || results.isEmpty
          then
            //println(s">> not match $target = $pattern, method = $methodMatch")
            find(xs)
          else
            val queries = parseQueries(x.path, getQuery(target))
            val query = queries |> mkQuery

            x.params match
              case None | Some(Nil) =>
                Some(RouteInfo(x, mkRoutePath(x.path, Nil), Params(Nil), query))
              case Some(pathParams) =>

                val params = mutable.ListBuffer[Param]()

                for i <- 1 to results.groupCount do
                  val paramVal = results.group(i)
                  val param = pathParams(i-1)
                  (param.regex.typ match
                    case RegexStr =>
                      ParamStr(param.name, paramVal)
                    case RegexInt =>
                      ParamInt(param.name, paramVal.toInt)
                    case RegexLong =>
                      ParamLong(param.name, paramVal.toLong)
                    case RegexPaths =>
                      ParamPaths(param.name, paramVal.split("/").toList)) |> params.append

                val pars = params.toSeq
                val route = mkRoutePath(x.path, pars)
                Some(RouteInfo(x, route, Params(pars), query))

        case _ => None

    find(routes.toList)

  private def parseQueries(path: Path, targetQuery: Option[Map[String, String]]): List[QueryParam] = targetQuery match
    case None => Nil
    case Some(qs) => path.query match
      case Nil =>
        qs.map { case (k, v) => QueryParam(k, QueryStr(v)) }.toList
      case dslQueries =>
        dslQueries.map {
          case RouteQueryInt(name) =>
            QueryParam(name, toRouteTypeOrError(findQueryValue(qs, name), toRouteIntOpt))
          case RouteQueryLong(name) =>
            QueryParam(name, toRouteTypeOrError(findQueryValue(qs, name), toRouteLongOpt))
          case RouteQueryStr(name) =>
            QueryParam(name, toRouteTypeOrError(findQueryValue(qs, name), toRouteStrOpt))
          case RouteQueryBool(name) =>
            QueryParam(name, toRouteTypeOrError(findQueryValue(qs, name), toRouteBoolOpt))
          case RouteQueryOpt(typ) =>
            typ match
              case RouteQueryList(typ) => QueryParam(getQueryName(typ), toRouteListOpt(qs, typ))
              case _ => QueryParam(getQueryName(typ), toRouteOpt(qs, typ))
          case RouteQueryList(typ) =>
            QueryParam(getQueryName(typ), toRouteListOpt(qs, typ))
        }

  private def toRouteIntOpt(v: String): Option[QueryInt] = v.toIntOption.map(QueryInt.apply)

  private def toRouteLongOpt(v: String): Option[QueryLong] = v.toLongOption.map(QueryLong.apply)

  private def toRouteStrOpt(v: String): Option[QueryStr] = Option.when(v.nonEmpty)(v).map(QueryStr.apply)

  private def toRouteBoolOpt(v: String): Option[QueryBool] = v.toBooleanOption.map(QueryBool.apply)

  private def toRouteList[T](v: Option[String], f: String => Option[T]): List[Option[T]] = v match
    case Some(s) => s.split(",").toList.map(f)
    case None => Nil

  private def toRouteTypeOrError[T <: QueryType](v: Option[String], f: String => Option[T]): QueryType = v match
    case Some(s) => f(s).getOrElse(QueryInvalid(s"invalid query value"))
    case None => QueryInvalid(s"query is required")

  private def unwrapRouteTypeOpt[T <: QueryType](v: Option[T]): T | QueryInvalid = v match
    case Some(r) => r
    case None => QueryInvalid(s"query is required")

  private def toRouteListOpt(qs: Map[String, String], typ: RouteQueryListType): QueryType = typ match
    case RouteQueryInt(name) =>
      toRouteList(findQueryValue(qs, name), toRouteIntOpt).map(unwrapRouteTypeOpt) |> QueryList.apply
    case RouteQueryLong(name) =>
      toRouteList(findQueryValue(qs, name), toRouteLongOpt).map(unwrapRouteTypeOpt) |> QueryList.apply
    case RouteQueryStr(name) =>
      toRouteList(findQueryValue(qs, name), toRouteStrOpt).map(unwrapRouteTypeOpt) |> QueryList.apply
    case _ => QueryInvalid(s"invalid query list parse")

  private def toRouteOpt(qs: Map[String, String], typ: RouteQueryOptType): QueryOption = typ match
    case RouteQueryInt(name) =>
      findQueryValue(qs, name).flatMap(toRouteIntOpt) |> QueryOption.apply
    case RouteQueryStr(name) =>
      findQueryValue(qs, name).flatMap(toRouteStrOpt) |> QueryOption.apply
    case RouteQueryLong(name) =>
      findQueryValue(qs, name).flatMap(toRouteLongOpt) |> QueryOption.apply
    case RouteQueryBool(name) =>
      findQueryValue(qs, name).flatMap(toRouteBoolOpt) |> QueryOption.apply
    case RouteQueryList(typ) =>
      toRouteListOpt(qs, typ) match
        case ql: QueryList => Some(ql) |> QueryOption.apply
        case _ => None |> QueryOption.apply

  private def getQueryName(typ: RouteQueryListType | RouteQueryOptType): String = typ match
    case RouteQueryInt(name) => name
    case RouteQueryLong(name) => name
    case RouteQueryStr(name) => name
    case RouteQueryBool(name) => name

  private def findQueryValue(qs: Map[String, String], name: String): Option[String] =
    qs.find((k, _) => k == name).map(_._2)

  private def getQuery(target: String): Option[Map[String, String]] =
    target.split('?').lastOption match
      case Some(queries) =>
        queries
          .split("&")
          .foldRight(Map[String, String]()) {
            (value, acc) =>
              value.split("=").toList match
                case k :: v :: Nil =>
                  acc + ((k, v))
                case _ => acc
          } |> Some.apply

      case None => None

  private def mkQuery(queries: List[QueryParam]): Query =

    print(s"queries = ${queries}")

    def extract(v: QueryType): Any = v match
      case QueryInt(v) => v
      case QueryLong(v) => v
      case QueryStr(v) => v
      case QueryBool(v) => v
      case QueryList(v) => v.map(extract)
      case QueryOption(v) => v.map(extract)

    val matcher =
      queries.map:
        case QueryParam(name, value) =>
          extract(value).asInstanceOf[QueryMatcherType]
    Query(queries, matcher)

  private def mkRoutePath(path: Path, params: Seq[Param]): RoutePath =
    val matcher: RouteMatcher =
      path.parts.map:
        case p: PathParam =>
          params.find(_.name == p.name) match
            case Some(ParamInt(_, v)) => v
            case Some(ParamLong(_, v)) => v
            case Some(ParamStr(_, v)) => v
            case Some(ParamPaths(_, tail)) => tail.head
            case _ => throw RouteException(s"param ${p.name} not found on route match creator")
        case p: PathRoot => p
        case p: PathEnd => p
        case PathPart(v) => v
        case p => throw RouteException(s"wrong path part ${p} on route match creator")
    path.parts.lastOption match
      case Some(ParamPaths(_, tail)) => RoutePath(path, matcher ::: tail.tail)
      case _ => RoutePath(path, matcher)