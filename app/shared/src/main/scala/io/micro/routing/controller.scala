package io.micro.routing

sealed trait ControllerBase:
  type Req <: RouteRequest
  type Resp <: RouteResponse

trait Controller extends ControllerBase:


  def get(req: Req): Resp

  def post(req: Req): Resp

  def put(req: Req): Resp

  def delete(req: Req): Resp

  def head(req: Req): Resp

  def options(req: Req): Resp

  def patch(req: Req): Resp

trait ControllerAsync extends ControllerBase:

  def get(req: Req, f: Resp => Unit): Unit

  def post(req: Req, f: Resp => Unit): Unit

  def put(req: Req, f: Resp => Unit): Unit

  def delete(req: Req, f: Resp => Unit): Unit

  def head(req: Req, f: Resp => Unit): Unit

  def options(req: Req, f: Resp => Unit): Unit

  def patch(req: Req, f: Resp => Unit): Unit

trait ControllerDispatcher extends ControllerBase:

  def dispatch(req: Req) : Resp

trait ControllerAsyncDispatcher extends ControllerBase:
  def dispatch(req: Req, f: Resp => Unit): Unit
