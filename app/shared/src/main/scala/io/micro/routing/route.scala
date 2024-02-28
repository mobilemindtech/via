package io.micro.routing

import scala.collection.mutable


case class Params(raw: Seq[Param] = Nil):
  def int(name: String): Option[Int] = raw.find(_.name == name) match
    case Some(ParamInt(_, i)) => Some(i)
    case _ => None

  def long(name: String): Option[Long] = raw.find(_.name == name) match
    case Some(ParamLong(_, i)) => Some(i)
    case _ => None

  def str(name: String): Option[String] = raw.find(_.name == name) match
    case Some(ParamStr(_, i)) => Some(i)
    case _ => None

  def paths(name: String): List[String] = raw.find(_.name == name) match
    case Some(ParamPaths(_, i)) => i
    case _ => Nil

  def tail(name: String): List[String] = paths(name)

  def size: Int = raw.size

  def indices: Range = raw.indices

  def at(i: Int): Param = raw(i)

case class Query(raw: List[QueryParam] = Nil, matcher: QueryMatcher = Nil)

case class RoutePath(path: Path = Path(), matcher: RouteMatcher = Nil)

trait RouteRequest(val method: Method,
                   val uri: String,
                   val route: RoutePath,
                   val params: Params,
                   val query: Query)

trait RouteResponse

type RouteCallbackOpt[Req <: RouteRequest, Resp <: RouteResponse] = Req => Option[Resp]
type RouteCallback[Req <: RouteRequest, Resp <: RouteResponse] = Req => Resp
type RouteAsyncCallback[Req <: RouteRequest, Resp <: RouteResponse]  = (Req, Resp => Unit) => Unit

trait Route(val method: Method,
            val path: Path,
            val next: Option[Middleware[_, _]] = None,
            val prev: Option[Middleware[_, _]] = None,
            val pattern: Option[String] = None,
            val params: Option[List[PathParam]] = None):

  infix def / (p: Path): Path =
    path.copy(parts = path.parts ::: p.parts)


  def copyWithNext(middleware: Middleware[_, _]) : Route

  def copyWithPrev(middleware: Middleware[_, _]) : Route

  def makeCompiled(pattern: String, params: List[PathParam]): Route

  def ++ (middleware: Middleware[_, _]): Route =
    copyWithNext(middleware)

  def compile : Route =

    val params = mutable.ListBuffer[PathParam]()

    //println(path.parts)

    val routeParts =
      path.parts
        .map[String]:
          case _: PathRoot => ""
          case _: PathEnd => "/$"
          case _: PathAny => "/(.+)$"
          case p : PathPart => s"/${p.path}"
          case p : PathParam =>
            params.append(p)
            p.regex match
              case PathParamRegex(_, pattern, _) => s"/($pattern)" // (group)
        .filterNot(_.isEmpty)

    val joined =
      routeParts match
        case Nil => "^/$"
        case _ =>
          val str = routeParts.mkString
          if str.endsWith("$")
          then s"^$str"
          else s"^$str$$"

    makeCompiled(joined, params.toList)


case class RouteDispatcher[Req <: RouteRequest, Resp <: RouteResponse](override val method: Method,
                                                                          override val path: Path,
                                                                          override val next: Option[Middleware[_, _]] = None,
                                                                          override val prev: Option[Middleware[_, _]] = None,
                                                                          override val pattern: Option[String] = None,
                                                                          override val params: Option[List[PathParam]] = None,
                                                                          dispatch: Option[RouteCallback[Req, Resp]] = None,
                                                                          dispatchAsync: Option[RouteAsyncCallback[Req, Resp]] = None,
                                                                        )  extends Route(method, path, next, prev):

  override def copyWithNext(middleware: Middleware[_, _]): Route =
    copy(next = Some(middleware))

  override def copyWithPrev(middleware: Middleware[_, _]): Route =
    copy(prev = Some(middleware))

  override def makeCompiled(pattern: String, params: List[PathParam]): Route =
    copy(pattern = Some(pattern), params = Some(params))

case class RouteController[RouteCtrl <: ControllerBase](override val method: Method,
                                                         override val path: Path,
                                                         override val next: Option[Middleware[_, _]] = None,
                                                         override val prev: Option[Middleware[_, _]] = None,
                                                         override val pattern: Option[String] = None,
                                                         override val params: Option[List[PathParam]] = None,
                                                         controller: RouteCtrl
                                                       )  extends Route(method, path, next, prev):
  override def copyWithNext(middleware: Middleware[_, _]): Route =
    copy(next = Some(middleware))

  override def copyWithPrev(middleware: Middleware[_, _]): Route =
    copy(prev = Some(middleware))

  override def makeCompiled(pattern: String, params: List[PathParam]): Route =
    copy(pattern = Some(pattern), params = Some(params))