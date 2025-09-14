package io.via.types

import PathParamType.*
import RouteQuery.*

/** Route path representation
  */
sealed trait PathDir()

/** The path part representation
  * @param path
  *   Path part
  */
case class PathPart(path: String) extends PathDir

/** Path root (/) representation
  */
case class PathRoot() extends PathDir

/** Path end representation (/)
  */
case class PathEnd() extends PathDir

case class PathAny() extends PathDir

/** Path regex param
  * @param name
  *   Param name
  * @param regex
  *   Regex pattern
  */
case class PathParam(name: String, regex: PathParamRegex) extends PathDir

/** Path regex result type
  */
enum RegexType:
  case RegexStr, RegexInt, RegexLong, RegexPaths

case class PathParamRegex(name: String, pattern: String, typ: RegexType)

type RouteMatcher = List[PathRoot | PathEnd | String | Int | Long]

object PathParamType:

  /** Param of int type
    * @param name
    *   Param name
    * @return
    */
  def reInt(name: String): PathParamRegex =
    PathParamRegex(name, "[0-9]+", RegexType.RegexInt)

  /** Param of long type
    * @param name
    *   Param name
    * @return
    */
  def reLong(name: String): PathParamRegex =
    PathParamRegex(name, "[0-9]+", RegexType.RegexLong)

  /** Param of any type
    * @param name
    *   Param name
    * @return
    */
  def reAny(name: String): PathParamRegex =
    PathParamRegex(name, ".+", RegexType.RegexStr)

  /** Param of list of paths
    *
    * @param name
    *   Param name
    * @return
    */
  def rePaths(name: String): PathParamRegex =
    PathParamRegex(name, ".+", RegexType.RegexPaths)

  /** Param of generic regex
    *
    * @param name
    *   Param name
    * @return
    */
  def reRaw(name: String, value: String): PathParamRegex =
    PathParamRegex(name, value, RegexType.RegexStr)

/** Path rules
  * @param parts
  *   Path parts
  * @param query
  *   Queries
  */
case class Path(parts: List[PathDir] = Nil, query: List[RouteQuery] = Nil):

  infix def /(p: String): Path =
    copy(parts = parts :+ PathPart(p))

  infix def /(p: Path): Path =
    copy(parts = parts ::: p.parts)

  infix def /(p: PathParam): Path =
    copy(parts = parts :+ p)

  /** Queries start
    * @param value
    * @return
    */
  infix def ?(value: RouteQueryVal): Path =
    copy(query = value :: Nil)

  /** Queries start
    * @param value
    * @return
    */
  infix def /?(value: RouteQueryVal): Path =
    copy(parts = parts :+ PathEnd(), query = value :: Nil)

  /** Another query
    * @param value
    * @return
    */
  infix def &(value: RouteQueryVal): Path =
    copy(query = query :+ value)

object Path:
  def apply(parts: PathDir*): Path = Path(parts.toList)

  def apply(q: RouteQuery): Path = Path(query = q :: Nil)

  def unapply(p: Path): Option[Seq[PathDir]] = Some(p.parts)

  infix def root: Path =
    Path(PathRoot())

  infix def any: Path =
    Path(PathAny())

  infix def /(route: Route, path: Path): Path =
    route.path / path

  infix def /(route: Route, path: String): Path =
    route.path / path

  infix def int(name: String): Path =
    pureRegex(reInt(name))

  infix def long(name: String): Path =
    pureRegex(reLong(name))

  infix def param(name: String): Path =
    pureRegex(reAny(name))

  infix def tail(name: String): Path =
    paths(name)

  infix def paths(name: String): Path =
    pureRegex(rePaths(name))

  infix def regex(name: String, value: String): Path =
    pureRegex(reRaw(name, value))

  private def pureRegex(regex: PathParamRegex): Path =
    Path(PathParam(regex.name, regex))
