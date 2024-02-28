package io.micro.routing

type MiddlewareDispatcher[Req <: RouteRequest, Resp <: RouteResponse] = Req => Resp

case class Middleware[Req <: RouteRequest, Resp <: RouteResponse](method: Method,
                                                                    dispatch: MiddlewareDispatcher[Req, Resp],
                                                                    prev: Option[Middleware[_, _]] = None,
                                                                    next: Option[Middleware[_, _]] = None):

  def ++ (route: Route): Route =
    route.copyWithPrev(this)

  def ++ (m: Middleware[_, _]): Middleware[_, _] =
    copy(next = Some(m))

