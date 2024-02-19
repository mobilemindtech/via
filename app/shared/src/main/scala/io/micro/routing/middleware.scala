package io.micro.routing

type MiddlewareDispatcher[TReq <: RouteRequest, TResp <: RouteResponse] = TReq => TResp

case class Middleware[TReq <: RouteRequest, TResp <: RouteResponse](method: Method,
                                                                    dispatch: MiddlewareDispatcher[TReq, TResp],
                                                                    prev: Option[Middleware[_, _]] = None,
                                                                    next: Option[Middleware[_, _]] = None):

  def ++ (route: Route): Route =
    route.copyWithPrev(this)

  def ++ (m: Middleware[_, _]): Middleware[_, _] =
    copy(next = Some(m))

