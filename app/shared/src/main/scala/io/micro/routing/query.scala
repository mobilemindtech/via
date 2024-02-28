package io.micro.routing

sealed trait RouteQuery


sealed trait RouteQueryVal(val name: String = "") extends RouteQuery

case class RouteQueryStr(override val name: String) extends RouteQueryVal(name)

case class RouteQueryInt(override val  name: String) extends RouteQueryVal(name)

case class RouteQueryLong(override val  name: String) extends RouteQueryVal(name)

case class RouteQueryBool(override val  name: String) extends RouteQueryVal(name)

type RouteQueryOptType = RouteQueryStr | RouteQueryInt | RouteQueryLong | RouteQueryBool | RouteQueryList

type RouteQueryListType = RouteQueryStr | RouteQueryInt | RouteQueryLong | RouteQueryBool

case class RouteQueryOpt(typ: RouteQueryOptType) extends RouteQueryVal

case class RouteQueryList(typ: RouteQueryListType) extends RouteQueryVal

object RouteQuery:

  def q_int(name: String): RouteQueryVal = RouteQueryInt(name)

  def q_str(name: String): RouteQueryVal = RouteQueryStr(name)

  def q_long(name: String): RouteQueryVal = RouteQueryLong(name)

  def q_bool(name: String): RouteQueryVal = RouteQueryBool(name)

  def q_list_int(name: String): RouteQueryVal = RouteQueryList(RouteQueryInt(name))

  def q_list_str(name: String): RouteQueryVal = RouteQueryList(RouteQueryStr(name))

  def q_list_long(name: String): RouteQueryVal = RouteQueryList(RouteQueryLong(name))

  def q_int_opt(name: String): RouteQueryVal = RouteQueryOpt(RouteQueryInt(name))

  def q_str_opt(name: String): RouteQueryVal = RouteQueryOpt(RouteQueryStr(name))

  def q_long_opt(name: String): RouteQueryVal = RouteQueryOpt(RouteQueryLong(name))

  def q_bool_opt(name: String): RouteQueryVal = RouteQueryOpt(RouteQueryBool(name))

  def q_list_str_opt(name: String): RouteQueryVal = RouteQueryOpt(RouteQueryList(RouteQueryStr(name)))

  def q_list_int_opt(name: String): RouteQueryVal = RouteQueryOpt(RouteQueryList(RouteQueryInt(name)))

  def q_list_long_opt(name: String): RouteQueryVal = RouteQueryOpt(RouteQueryList(RouteQueryLong(name)))



/*
import RouteQuery.*
import Path.*
import Method.*


def x =
  Path(Nil) ? q_int("id") & q_str_opt("id")

def y =
  root / "user" / int("id") ? q_int("id") & q_str_opt("name")
 */
