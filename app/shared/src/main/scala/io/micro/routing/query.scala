package io.micro.routing

import io.micro.routing.Query.{QueryMatcher, QueryParam}
import Query.QueryType._

object RouteQuery:

  sealed trait RouteQuery
  
  sealed trait RouteQueryVal(val name: String = "") extends RouteQuery
  
  case class RouteQueryStr(override val name: String) extends RouteQueryVal(name)
  
  case class RouteQueryRegex(override val name: String, pattern: String) extends RouteQueryVal(name)
  
  case class RouteQueryInt(override val  name: String) extends RouteQueryVal(name)
  
  case class RouteQueryLong(override val  name: String) extends RouteQueryVal(name)
  
  case class RouteQueryBool(override val  name: String) extends RouteQueryVal(name)
  
  type RouteQueryOptType =
    RouteQueryStr | RouteQueryRegex | RouteQueryInt | RouteQueryLong | RouteQueryBool | RouteQueryList
  
  type RouteQueryListType =
    RouteQueryStr | RouteQueryRegex | RouteQueryInt | RouteQueryLong | RouteQueryBool
  
  case class RouteQueryOpt(typ: RouteQueryOptType) extends RouteQueryVal
  
  case class RouteQueryList(typ: RouteQueryListType) extends RouteQueryVal

  def q_int(name: String): RouteQueryVal = RouteQueryInt(name)

  def q_str(name: String): RouteQueryVal = RouteQueryStr(name)

  def q_long(name: String): RouteQueryVal = RouteQueryLong(name)

  def q_bool(name: String): RouteQueryVal = RouteQueryBool(name)

  def q_regex(name: String, pattern: String): RouteQueryVal = RouteQueryRegex(name, pattern)

  def q_list_int(name: String): RouteQueryVal = RouteQueryList(RouteQueryInt(name))

  def q_list_str(name: String): RouteQueryVal = RouteQueryList(RouteQueryStr(name))

  def q_list_long(name: String): RouteQueryVal = RouteQueryList(RouteQueryLong(name))

  def q_int_opt(name: String): RouteQueryVal = RouteQueryOpt(RouteQueryInt(name))

  def q_str_opt(name: String): RouteQueryVal = RouteQueryOpt(RouteQueryStr(name))

  def q_regex_opt(name: String, pattern: String): RouteQueryVal = RouteQueryOpt(RouteQueryRegex(name, pattern))

  def q_long_opt(name: String): RouteQueryVal = RouteQueryOpt(RouteQueryLong(name))

  def q_bool_opt(name: String): RouteQueryVal = RouteQueryOpt(RouteQueryBool(name))

  def q_list_str_opt(name: String): RouteQueryVal = RouteQueryOpt(RouteQueryList(RouteQueryStr(name)))

  def q_list_int_opt(name: String): RouteQueryVal = RouteQueryOpt(RouteQueryList(RouteQueryInt(name)))

  def q_list_long_opt(name: String): RouteQueryVal = RouteQueryOpt(RouteQueryList(RouteQueryLong(name)))

case class Query(raw: List[QueryParam] = Nil, matcher: QueryMatcher = Nil)
object Query:
  enum QueryType:
    case QueryStr(v: String) extends QueryType
    case QueryRegex(v: String, regex: String) extends QueryType
    case QueryInt(v: Int) extends QueryType
    case QueryLong(v: Long) extends QueryType
    case QueryBool(v: Boolean) extends QueryType
    case QueryList(v: QueryListType)  extends QueryType
    case QueryOption(v: QueryOptType)  extends QueryType
    case QueryInvalid() extends QueryType
  
  type QueryListType =
    List[QueryStr | QueryRegex | QueryInt | QueryLong | QueryInvalid]
  
  type QueryOptType =
    Option[QueryStr | QueryRegex | QueryInt | QueryLong | QueryBool | QueryList]
  
  case class QueryParam(name: String, value: QueryType)
  
  type QueryMatcherListType = List[Int | Long | String]
  type QueryMatcherOptType = Option[Int | Long | String | Boolean | QueryMatcherListType]
  type QueryMatcherType =
    Int | Long | String | Boolean | QueryMatcherOptType | QueryMatcherListType
  
  type QueryMatcher = List[QueryMatcherType]
