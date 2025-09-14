package io.via.types

type MiddlewareEnter[Req, Resp] = Req => Req | Resp
type MiddlewareLeave[Req, Resp] = (Req, Resp) => Resp

/** Middleware representation
  * @tparam Req
  *   Request type
  * @tparam Resp
  *   Response Type
  */
sealed trait Middleware[+Req, +Resp]:
  /** Http methods allowed
    */
  val methods: Seq[Method]

/** Execute after route apply
  * @param methods
  *   Http methods allowed
  * @param handler
  *   Handle function
  * @param next
  *   Next middleware to apply
  * @tparam Req
  *   Request type
  * @tparam Resp
  *   Response Type
  */
case class Leave[Req, Resp](
    override val methods: Seq[Method],
    handler: MiddlewareLeave[Req, Resp],
    next: Option[Leave[Req, Resp]] = None
) extends Middleware[Req, Resp]:
  /** Concatenate next middleware to apply
    * @param md
    *   The middleware
    * @return
    *   Middleware chain
    */
  def ++(md: Leave[Req, Resp]): Leave[Req, Resp] =
    copy(next = Some(md))

/** Execute before route apply
  * @param methods
  *   Http methods allowed
  * @param handler
  *   Handle function
  * @param next
  *   Next middleware to apply
  * @tparam Req
  *   Request type
  * @tparam Resp
  *   Response Type
  */
case class Enter[Req, Resp](
    override val methods: Seq[Method],
    handler: MiddlewareEnter[Req, Resp],
    next: Option[Enter[Req, Resp]] = None
) extends Middleware[Req, Resp]:
  /** Concatenate next middleware to apply
    * @param md
    *   The middleware
    * @return
    *   Middleware chain
    */
  def ++(md: Enter[Req, Resp]): Enter[Req, Resp] =
    copy(next = Some(md))

  /** Concatenate route to apply
    * @param route
    *   The route
    * @return
    *   The route chain
    */
  def ++(route: RouteEntry[Req, Resp]): RouteEntry[Req, Resp] =
    route.copyWithEnter(this)
