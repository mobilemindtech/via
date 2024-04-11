package br.com.mobilemind.micro.routing.router

type MiddlewareBefore[Req, Resp] = Req => Req | Resp
type MiddlewareAfter[Req, Resp] = (Req, Resp) => Resp

sealed trait Middleware[+Req, +Resp]:
  val methods: Seq[Method]

// depois
case class After[Req, Resp](
    override val methods: Seq[Method],
    handler: MiddlewareAfter[Req, Resp],
    next: Option[After[Req, Resp]] = None
) extends Middleware[Req, Resp]:
  def ++(m: After[Req, Resp]): After[Req, Resp] =
    copy(next = Some(m))

// antes
case class Before[Req, Resp](
    override val methods: Seq[Method],
    handler: MiddlewareBefore[Req, Resp],
    next: Option[Before[Req, Resp]] = None
) extends Middleware[Req, Resp]:
  def ++(m: Before[Req, Resp]): Before[Req, Resp] =
    copy(next = Some(m))

  def ++(route: RouteEntry[Req, Resp]): RouteEntry[Req, Resp] =
    route.copyWithBefore(this)
