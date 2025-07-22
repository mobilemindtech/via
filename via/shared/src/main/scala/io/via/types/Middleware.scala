package io.via.types

type MiddlewareEnter[Req, Resp] = Req => Req | Resp
type MiddlewareLeave[Req, Resp] = (Req, Resp) => Resp

sealed trait Middleware[+Req, +Resp]:
  val methods: Seq[Method]

// depois
case class Leave[Req, Resp](
    override val methods: Seq[Method],
    handler: MiddlewareLeave[Req, Resp],
    next: Option[Leave[Req, Resp]] = None
) extends Middleware[Req, Resp]:
  def ++(m: Leave[Req, Resp]): Leave[Req, Resp] =
    copy(next = Some(m))

// antes
case class Enter[Req, Resp](
    override val methods: Seq[Method],
    handler: MiddlewareEnter[Req, Resp],
    next: Option[Enter[Req, Resp]] = None
) extends Middleware[Req, Resp]:
  def ++(m: Enter[Req, Resp]): Enter[Req, Resp] =
    copy(next = Some(m))

  def ++(route: RouteEntry[Req, Resp]): RouteEntry[Req, Resp] =
    route.copyWithEnter(this)
