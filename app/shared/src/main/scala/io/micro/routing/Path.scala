package io.micro.routing

import io.micro.routing.Path.paths
import io.micro.routing.PathParamType.*
import io.micro.routing.RouteQuery.{RouteQuery, RouteQueryVal}

sealed trait PathDir()

case class PathPart(path: String) extends PathDir

case class PathRoot() extends PathDir

case class PathEnd() extends PathDir

case class PathAny() extends PathDir

case class PathParam(name: String, regex: PathParamRegex) extends PathDir

enum RegexType:
  case RegexStr, RegexInt, RegexLong, RegexPaths

case class PathParamRegex(name: String, pattern: String, typ: RegexType)

type RouteMatcher = List[PathRoot | PathEnd | String | Int | Long]

object PathParamType:

  def regexint(name: String): PathParamRegex =
    PathParamRegex(name, "[0-9]+", RegexType.RegexInt)

  def regexlong(name: String): PathParamRegex =
    PathParamRegex(name, "[0-9]+", RegexType.RegexLong)

  def regexany(name: String): PathParamRegex =
    PathParamRegex(name, ".+", RegexType.RegexStr)

  def regexpaths(name: String): PathParamRegex =
    PathParamRegex(name, ".+", RegexType.RegexPaths)

  def regexraw(name: String, value: String): PathParamRegex =
    PathParamRegex(name, value, RegexType.RegexStr)

case class Path(parts: List[PathDir] = Nil, query: List[RouteQuery] = Nil):
  infix def / (p: String): Path =
    copy(parts = parts :+ PathPart(p))

  infix def / (p: Path): Path =
    copy(parts = parts ::: p.parts)

  infix def / (p: PathParam): Path =
    copy(parts = parts :+ p)

  infix def ? (value: RouteQueryVal): Path =
    copy(query = value :: Nil)

  infix def /? (value: RouteQueryVal): Path =
    copy(parts = parts :+ PathEnd(), query = value :: Nil)

  infix def & (value: RouteQueryVal): Path =
    copy(query = query :+ value)


object Path:
  def apply(parts: PathDir*): Path = Path(parts.toList)

  def apply(q: RouteQuery): Path = Path(query = q :: Nil)

  def unapply(p: Path): Option[Seq[PathDir]] = Some(p.parts)

  infix def root: Path =
    Path(PathRoot())

  infix def any: Path =
    Path(PathAny())

  infix def / (route: Route, path: Path): Path =
    route.path / path

  infix def / (route: Route, path: String): Path =
    route.path / path

  infix def int(name: String): Path =
    pureregex(regexint(name))

  infix def long(name: String): Path =
    pureregex(regexlong(name))

  infix def param(name: String): Path =
    pureregex(regexany(name))

  infix def tail(name: String): Path =
    paths(name)

  infix def paths(name: String): Path =
    pureregex(regexpaths(name))

  infix def regex(name: String, value: String): Path =
    pureregex(regexraw(name, value))

  private def pureregex(regex: PathParamRegex): Path =
    Path(PathParam(regex.name, regex))
