package io.micro.routing

sealed trait Param(val name: String)

case class ParamInt(override  val name: String, value: Int) extends Param(name)

case class ParamLong(override val name: String, value: Long) extends Param(name)

case class ParamStr(override val name: String, value: String) extends Param(name)

case class ParamPaths(override  val name: String, tail: List[String]) extends Param(name)
