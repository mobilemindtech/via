package io.micro.routing

trait PathDir()


sealed class PathPart(val path: String) extends PathDir

//sealed class PathSlash() extends PathPart("")

sealed class PathRoot() extends PathPart("")

sealed class PathEnd() extends PathPart("")

sealed class PathAny() extends PathPart("")

sealed class PathParam(val name: String, val regex: PathParamRegex) extends PathDir

enum RegexType:
  case RegexStr, RegexInt, RegexLong, RegexPaths

case class PathParamRegex(name: String, pattern: String, typ: RegexType)


object PathParamType:

  def regexint(name: String): PathParamRegex =
    PathParamRegex(name, "[0-9]+", RegexType.RegexInt)

  def regexlong(name: String): PathParamRegex =
    PathParamRegex(name, "[0-9]+", RegexType.RegexLong)

  def regexany(name: String): PathParamRegex =
    PathParamRegex(name, ".+", RegexType.RegexStr)

  def regexpaths(name: String): PathParamRegex =
    PathParamRegex(name, ".+", RegexType.RegexPaths)

case class Path(parts: List[PathDir]):
  infix def / (p: String): Path =
    copy(parts = parts :+ PathPart(p))

  infix def / (p: Path): Path =
    copy(parts = parts ::: p.parts)

  infix def / (p: PathParam): Path =
    copy(parts = parts :+ p)

object Path:
  def apply(parts: PathDir*): Path = Path(parts.toList)

  infix def root: Path =
    Path(PathRoot())

  infix def any: Path =
    Path(PathAny())
