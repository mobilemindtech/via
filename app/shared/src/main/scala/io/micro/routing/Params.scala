package io.micro.routing

sealed trait Param:
  val name: String

case class ParamInt(override val name: String,
                    value: Int) extends Param

case class ParamLong(override val name:
                     String, value: Long) extends Param

case class ParamStr(override val name: String,
                    value: String) extends Param

case class ParamPaths(override  val name: String,
                      tail: List[String]) extends Param

case class Params(raw: Seq[Param] = Nil):
  def int(name: String): Option[Int] = raw.find(_.name == name) match
    case Some(ParamInt(_, i)) => Some(i)
    case _ => None

  def long(name: String): Option[Long] = raw.find(_.name == name) match
    case Some(ParamLong(_, i)) => Some(i)
    case _ => None

  def str(name: String): Option[String] = raw.find(_.name == name) match
    case Some(ParamStr(_, i)) => Some(i)
    case _ => None

  def paths(name: String): List[String] = raw.find(_.name == name) match
    case Some(ParamPaths(_, i)) => i
    case _ => Nil

  def tail(name: String): List[String] = paths(name)

  def size: Int = raw.size

  def indices: Range = raw.indices

  def at(i: Int): Param = raw(i)

