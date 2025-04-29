package io.micro.router.types

case class RouteInfo(
  method: Method,
  target: String,
  matcher: RouteMatcher,
  params: Params,
  query: Query
)