package io.micro.router.core

import io.micro.router.core.Method

import scala.collection.mutable

case class Route(
    path: Path,
    pattern: String = "",
    pure: Boolean = false, // if has not params and query
    params: List[PathParam] = Nil,
    methods: Seq[Method] = Nil,
    tag: Option[Any] = None
):

  infix def /(p: Path): Path =
    path.copy(parts = path.parts ::: p.parts)

  def compile: Route =

    val params = mutable.ListBuffer[PathParam]()

    val routeParts =
      path.parts
        .map[String]:
          case _: PathRoot => ""
          case _: PathEnd  => "/$"
          case _: PathAny  => "/(.+)$"
          case p: PathPart => s"/${p.path}"
          case p: PathParam =>
            params.append(p)
            p.regex match
              case PathParamRegex(_, pattern, _) => s"/($pattern)" // (group)
        .filterNot(_.isEmpty)

    val ptrn =
      routeParts match
        case Nil => "^/$"
        case _ =>
          val str = routeParts.mkString
          if str.endsWith("$")
          then s"^$str"
          else s"^$str$$"

    val pars = params.toList
    copy(pattern = ptrn, params = pars, pure = pars.isEmpty)

object Route:

  def route(path: Path): Route =
    Route(path).compile
