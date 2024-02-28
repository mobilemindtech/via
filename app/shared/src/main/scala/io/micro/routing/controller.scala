package io.micro.routing

sealed trait ControllerBase


trait Controller[Req <: RouteRequest, Resp <: RouteResponse] extends ControllerBase:

  def get(req: Req): Option[Resp] = None

  def post(req: Req): Option[Resp] = None

  def put(req: Req): Option[Resp] = None

  def delete(req: Req): Option[Resp] = None

  def head(req: Req): Option[Resp] = None

  def options(req: Req): Option[Resp] = None

  def patch(req: Req): Option[Resp] = None

trait ControllerAsync[Req <: RouteRequest, Resp <: RouteResponse] extends ControllerBase:

  def get(req: Req, f: Resp => Unit): Unit = ()

  def post(req: Req, f: Resp => Unit): Unit = ()

  def put(req: Req, f: Resp => Unit): Unit = ()

  def delete(req: Req, f: Resp => Unit): Unit = ()

  def head(req: Req, f: Resp => Unit): Unit = ()

  def options(req: Req, f: Resp => Unit): Unit = ()

  def patch(req: Req, f: Resp => Unit): Unit = ()

trait ControllerDispatcher[Req <: RouteRequest, Resp <: RouteResponse] extends ControllerBase:

  def dispatch(req: Req) : Resp

trait ControllerAsyncDispatcher[Req <: RouteRequest, Resp <: RouteResponse] extends ControllerBase:
  def dispatch(req: Req, f: Resp => Unit): Unit
