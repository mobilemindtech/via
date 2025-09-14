package io.via.core

import io.via.types.*
import io.via.types.Path.{int, param, regex, root, tail}

import scala.reflect.TypeTest
import io.via.types.Method.*
import RouteChain.*
import io.via.types.{
  Controller,
  Dispatcher,
  Enter,
  Handler,
  Leave,
  Method,
  MiddlewareEnter,
  MiddlewareLeave,
  Path,
  Route,
  RouteEntry,
  RouteEntryController,
  RouteEntryDispatcher,
  RouteEntryHandler,
  RouteInfo
}

import scala.annotation.tailrec

/** Rule to create a new Request, base on route info and route extra
  * information. Route extra is the necessary low-level information to create a
  * new Request. This should be provided by server implementation.
  * @tparam Req
  *   Request type
  * @tparam Extra
  *   Route extra type
  */
trait RequestBuilder[Req, Extra]:

  def build(routeInfo: RouteInfo, routeExtra: Option[Extra]): Req

case class Router[Req, Resp, Extra](routes: RouteEntry[Req, Resp]*)(using
    requestBuilder: RequestBuilder[Req, Extra],
    ttReq: TypeTest[Any, Req],
    ttResp: TypeTest[Any, Resp]
):

  /** Dispatch route
    * @param target
    *   Target URL
    * @param extra
    *   Route extra to create request
    * @return
    *   The response
    */
  def dispatch(target: String, extra: Extra): Option[Resp] =
    dispatch(ANY, target, extra)

  /** Dispatch route
    * @param method
    *   Http Method
    * @param target
    *   Target URL
    * @param extra
    *   Route extra to create request
    * @return
    *   The response
    */
  def dispatch(method: Method, target: String, extra: Extra): Option[Resp] =
    doRequest(method, target, Some(extra))

  /** Dispatch route
    * @param target
    *   Target URL
    * @return
    *   The response
    */
  def dispatch(target: String): Option[Resp] =
    dispatch(ANY, target)

  /** Dispatch route
    * @param method
    *   Http method
    * @param target
    *   Target URL
    * @return
    *   The response
    */
  def dispatch(
      method: Method,
      target: String
  ): Option[Resp] =
    doRequest(method, target, None)

  private def doRequest(
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
        doRequest(method, target, routeFound, extra)
      case RouteNotFound() => None

  private def doRequest(
      method: Method,
      target: String,
      routeFound: RouteFound,
      extra: Option[Extra]
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
    val req = requestBuilder.build(routeInfo, extra)
    val resp =
      entry match
        case r: RouteEntryHandler[Req, Resp] =>
          val resp =
            applyEnter(method, req, r.enter) match
              case newReq: Req =>
                r.handler.handle(newReq)
              case resp: Resp => resp

          resp |> Some.apply

        case r: RouteEntryDispatcher[Req, Resp] =>
          val resp =
            applyEnter(method, req, r.enter) match
              case ttReq(newReq) =>
                r.dispatcher(newReq)
              case ttResp(resp) => resp

          resp |> Some.apply

        case r: RouteEntryController[Req, Resp] =>
          applyEnter(method, req, r.enter) match
            case ttResp(resp) => resp |> Some.apply
            case ttReq(newReq) =>
              val crlResult =
                method match
                  case GET     => r.controller.get(newReq)
                  case POST    => r.controller.post(newReq)
                  case PUT     => r.controller.put(newReq)
                  case DELETE  => r.controller.delete(newReq)
                  case OPTIONS => r.controller.options(newReq)
                  case HEAD    => r.controller.head(newReq)
                  case PATCH   => r.controller.patch(newReq)
                  case CONNECT => r.controller.patch(newReq)
                  case TRACE   => r.controller.patch(newReq)
                  case ANY     => ()
              crlResult match
                case resp: Resp => resp |> Some.apply
                case _          => // not found
                  // try default handler
                  r.controller.handle(newReq) match
                    case resp: Resp => resp |> Some.apply
                    case _          => None // not found

    resp match
      case Some(rsp) =>
        applyLeave(method, req, rsp, entry.leave) |> Some.apply
      case None => None

  /** Apply enter middleware
    * @param method
    * @param req
    * @param enterOpt
    * @return
    */
  @tailrec
  private def applyEnter(
      method: Method,
      req: Req,
      enterOpt: Option[Enter[Req, Resp]]
  ): Resp | Req =
    enterOpt match
      case Some(enter) =>
        if enter.methods.exists(m => m == ANY || m == method)
        then
          enter.handler(req) match
            case ttReq(newReq) =>
              applyEnter(method, newReq, enter.next)
            case ttResp(resp) =>
              resp
        else applyEnter(method, req, enter.next)
      case _ => req

  /** Apply leave middleware
    * @param method
    * @param req
    * @param resp
    * @param leaveOpt
    * @return
    */
  @tailrec
  private def applyLeave(
      method: Method,
      req: Req,
      resp: Resp,
      leaveOpt: Option[Leave[Req, Resp]]
  ): Resp =
    leaveOpt match
      case Some(leave) =>
        val newResp =
          if leave.methods.exists(m => m == ANY || m == method)
          then leave.handler(req, resp)
          else resp
        applyLeave(method, req, newResp, leave.next)
      case _ => resp

object Router:

  def verbs(methods: Method*): Seq[Method] = methods

  def route[Req, Resp](
      path: Path,
      c: Controller[Req, Resp]
  ): RouteEntry[Req, Resp] =
    route(ANY, path, c)

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
    RouteEntryController(methods, route(path), controller = c)

  def route[Req, Resp](
      method: Method,
      path: String,
      c: Controller[Req, Resp]
  ): RouteEntry[Req, Resp] =
    RouteEntryController(method :: Nil, route(path), controller = c)

  def route[Req, Resp](path: Path)(
      c: Handler[Req, Resp]
  ): RouteEntry[Req, Resp] =
    route(ANY, path)(c)

  def route[Req, Resp](method: Method, path: Path)(
      c: Handler[Req, Resp]
  ): RouteEntry[Req, Resp] =
    route(method :: Nil, path)(c)

  def route[Req, Resp](methods: Seq[Method], path: Path)(
      f: Handler[Req, Resp]
  ): RouteEntry[Req, Resp] =
    RouteEntryHandler(methods, route(path), handler = f)

  def route[Req, Resp](method: Method, path: String)(
      f: Handler[Req, Resp]
  ): RouteEntry[Req, Resp] =
    RouteEntryHandler(method :: Nil, route(path), handler = f)

  def route[Req, Resp](path: Path)(
      f: Dispatcher[Req, Resp]
  ): RouteEntry[Req, Resp] =
    route(ANY, path)(f)

  def route[Req, Resp](method: Method, path: Path)(
      f: Dispatcher[Req, Resp]
  ): RouteEntry[Req, Resp] =
    route(method :: Nil, path)(f)

  def route[Req, Resp](methods: Seq[Method], path: Path)(
      f: Dispatcher[Req, Resp]
  ): RouteEntry[Req, Resp] =
    RouteEntryDispatcher(methods, route(path), dispatcher = f)

  def route[Req, Resp](method: Method, path: String)(
      f: Dispatcher[Req, Resp]
  ): RouteEntry[Req, Resp] =
    RouteEntryDispatcher(method :: Nil, route(path), dispatcher = f)

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

  def leave[Req, Resp](methods: Method*)(
      dispatch: MiddlewareLeave[Req, Resp]
  ): Leave[Req, Resp] =
    Leave(methods, dispatch)

  def leave[Req, Resp](dispatch: MiddlewareLeave[Req, Resp]): Leave[Req, Resp] =
    Leave(ANY :: Nil, dispatch)

  def enter[Req, Resp](methods: Method*)(
      dispatch: MiddlewareEnter[Req, Resp]
  ): Enter[Req, Resp] =
    Enter(methods, dispatch)

  def enter[Req, Resp](
      dispatch: MiddlewareEnter[Req, Resp]
  ): Enter[Req, Resp] =
    Enter(ANY :: Nil, dispatch)

  // string path

  def route(path: Path): Route =
    Route(path).compile

  /** Analyze route path
    * @param path
    *   E.g. / /user/:id -> any value (string param) /user/:id([0-9]) -> int
    *   value (int param) /user/:id(int) -> int value (int param) /user/\* ->
    *   tail paths (seq param) /user/:id([0-9]+) -> regrex value (string param)
    *
    * @return
    */
  def route(path: String): Route =
    if path == "/"
    then route(root)
    else
      val parts =
        path.split("/").foldLeft(root) { (acc: Path, part: String) =>
          if part.isEmpty
          then acc
          else
            val re = "^:(\\w+)(\\(.*\\))?".r
            re.findFirstMatchIn(part) match
              case Some(matcher) =>
                val varName = matcher.group(1)
                matcher.group(2) match
                  case null                             => acc / param(varName)
                  case "([0-9])" | "([0-9]+)" | "(int)" => acc / int(varName)
                  case value => acc / regex(varName, value)
              case _ =>
                part.charAt(0) match
                  case '*' =>
                    acc / tail("paths")
                  case _ =>
                    acc / part
        }
      route(parts)
