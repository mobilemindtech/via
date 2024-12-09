package io.micro.router.core

import Query.{QueryListType, QueryMatcher, QueryParam}
import Query.QueryType.*

import scala.reflect.TypeTest

object RouteQuery:

  sealed trait RouteQuery

  sealed trait RouteQueryVal extends RouteQuery:
    val name: String = ""

  case class RouteQueryStr(override val name: String) extends RouteQueryVal

  case class RouteQueryRegex(override val name: String, pattern: String)
      extends RouteQueryVal

  case class RouteQueryInt(override val name: String) extends RouteQueryVal

  case class RouteQueryLong(override val name: String) extends RouteQueryVal

  case class RouteQueryBool(override val name: String) extends RouteQueryVal

  type RouteQueryOptType =
    RouteQueryStr | RouteQueryRegex | RouteQueryInt | RouteQueryLong |
      RouteQueryBool | RouteQueryList

  type RouteQueryListType =
    RouteQueryStr | RouteQueryRegex | RouteQueryInt | RouteQueryLong |
      RouteQueryBool

  case class RouteQueryOpt(typ: RouteQueryOptType) extends RouteQueryVal

  case class RouteQueryList(typ: RouteQueryListType) extends RouteQueryVal

  def q_int(name: String): RouteQueryVal = RouteQueryInt(name)

  def q_str(name: String): RouteQueryVal = RouteQueryStr(name)

  def q_long(name: String): RouteQueryVal = RouteQueryLong(name)

  def q_bool(name: String): RouteQueryVal = RouteQueryBool(name)

  def q_regex(name: String, pattern: String): RouteQueryVal =
    RouteQueryRegex(name, pattern)

  def q_list_int(name: String): RouteQueryVal = RouteQueryList(
    RouteQueryInt(name)
  )

  def q_list_str(name: String): RouteQueryVal = RouteQueryList(
    RouteQueryStr(name)
  )

  def q_list_long(name: String): RouteQueryVal = RouteQueryList(
    RouteQueryLong(name)
  )

  def q_int_opt(name: String): RouteQueryVal = RouteQueryOpt(
    RouteQueryInt(name)
  )

  def q_str_opt(name: String): RouteQueryVal = RouteQueryOpt(
    RouteQueryStr(name)
  )

  def q_regex_opt(name: String, pattern: String): RouteQueryVal = RouteQueryOpt(
    RouteQueryRegex(name, pattern)
  )

  def q_long_opt(name: String): RouteQueryVal = RouteQueryOpt(
    RouteQueryLong(name)
  )

  def q_bool_opt(name: String): RouteQueryVal = RouteQueryOpt(
    RouteQueryBool(name)
  )

  def q_list_str_opt(name: String): RouteQueryVal = RouteQueryOpt(
    RouteQueryList(RouteQueryStr(name))
  )

  def q_list_int_opt(name: String): RouteQueryVal = RouteQueryOpt(
    RouteQueryList(RouteQueryInt(name))
  )

  def q_list_long_opt(name: String): RouteQueryVal = RouteQueryOpt(
    RouteQueryList(RouteQueryLong(name))
  )

case class Query(
    raw: List[QueryParam] = Nil,
    matcher: QueryMatcher = Nil,
    tuple: Seq[(String, Any)] = Nil
):

  def size: Int = raw.size

  def empty: Boolean = size == 0

  def nonEmpty: Boolean = !empty

  def listStr(name: String)(using
      t: TypeTest[Any, List[String]]
  ): List[String] =
    tuple.find(_._1 == name) match
      case Some((_, l @ t(v)))       => l
      case Some((_, Some(l @ t(v)))) => l
      case _                         => Nil

  def listInt(name: String)(using t: TypeTest[Any, List[Int]]): List[Int] =
    tuple.find(_._1 == name) match
      case Some((_, l @ t(v)))       => l
      case Some((_, Some(l @ t(v)))) => l
      case _                         => Nil

  def listLong(name: String)(using t: TypeTest[Any, List[Long]]): List[Long] =
    tuple.find(_._1 == name) match
      case Some((_, l @ t(v)))       => l
      case Some((_, Some(l @ t(v)))) => l
      case _                         => Nil

  def str(name: String): Option[String] =
    tuple.find(_._1 == name) match
      case Some((_, s: String))       => Some(s)
      case Some((_, Some(s: String))) => Some(s)
      case _                          => None

  def int(name: String): Option[Int] =
    tuple.find(_._1 == name) match
      case Some((_, s: Int))       => Some(s)
      case Some((_, Some(s: Int))) => Some(s)
      case _                       => None

  def long(name: String): Option[Long] =
    tuple.find(_._1 == name) match
      case Some((_, s: Long))       => Some(s)
      case Some((_, Some(s: Long))) => Some(s)
      case _                        => None

  def bool(name: String): Option[Boolean] =
    tuple.find(_._1 == name) match
      case Some((_, s: Boolean)) => Some(s)
      case _                     => None

object Query:
  enum QueryType:
    case QueryStr(v: String) extends QueryType
    case QueryInt(v: Int) extends QueryType
    case QueryLong(v: Long) extends QueryType
    case QueryBool(v: Boolean) extends QueryType
    case QueryList(v: QueryListType) extends QueryType
    case QueryOption(v: QueryOptType) extends QueryType
    case QueryInvalid() extends QueryType

  type QueryListType =
    List[QueryStr | QueryInt | QueryLong | QueryInvalid]

  type QueryOptType =
    Option[QueryStr | QueryInt | QueryLong | QueryBool | QueryList]

  case class QueryParam(name: String, value: QueryType)

  type QueryMatcherListType = List[Int | Long | String]
  type QueryMatcherOptType =
    Option[Int | Long | String | Boolean | QueryMatcherListType]
  type QueryMatcherType =
    Int | Long | String | Boolean | QueryMatcherOptType | QueryMatcherListType

  type QueryMatcher = List[QueryMatcherType]
