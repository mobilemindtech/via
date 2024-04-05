package io.micro.routing.router


import io.micro.routing.Route

enum Method:
  case Get, Post, Put, Delete, Patch, Head, Options, Any

type Dispatcher[Req, Resp] = Req => Resp

trait RouteEntry[Req, Resp]:
  val after: Option[After[Req, Resp]] = None
  val before: Option[Before[Req, Resp]] = None
  val method: Method
  val route: Route

  def copyWithAfter(m: After[Req, Resp]): RouteEntry[Req, Resp]
  def copyWithBefore(m: Before[Req, Resp]): RouteEntry[Req, Resp]

  def ++ (m: After[Req, Resp]): RouteEntry[Req, Resp] =
    copyWithAfter(m)

case class RouteEntryHandler[Req, Resp](override val method: Method,
                                                                         override val route: Route,
                                                                         override val after: Option[After[Req, Resp]] = None,
                                                                         override val before: Option[Before[Req, Resp]] = None,
                                                                         handler: Handler[Req, Resp])
  extends RouteEntry[Req, Resp]:

  override def copyWithAfter(m: After[Req, Resp]): RouteEntry[Req, Resp] =
    copy(after = Some(m))

  override def copyWithBefore(m: Before[Req, Resp]): RouteEntry[Req, Resp] =
    copy(before = Some(m))

case class RouteEntryController[Req, Resp](override val method: Method,
                                                                            override val route: Route,
                                                                            override val after: Option[After[Req, Resp]] = None,
                                                                            override val before: Option[Before[Req, Resp]] = None,
                                                                            controller: Controller[Req, Resp])
  extends RouteEntry[Req, Resp]:

  override def copyWithAfter(m: After[Req, Resp]): RouteEntry[Req, Resp] =
    copy(after = Some(m))

  override def copyWithBefore(m: Before[Req, Resp]): RouteEntry[Req, Resp] =
    copy(before = Some(m))


case class RouteEntryDispatcher[Req, Resp](override val method: Method,
                                                                            override val route: Route,
                                                                            override val after: Option[After[Req, Resp]] = None,
                                                                            override val before: Option[Before[Req, Resp]] = None,
                                                                            dispatcher: Dispatcher[Req, Resp])
  extends RouteEntry[Req, Resp]:

  override def copyWithAfter(m: After[Req, Resp]): RouteEntry[Req, Resp] =
    copy(after = Some(m))

  override def copyWithBefore(m: Before[Req, Resp]): RouteEntry[Req, Resp] =
    copy(before = Some(m))


