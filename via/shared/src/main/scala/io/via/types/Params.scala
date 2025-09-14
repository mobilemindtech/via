package io.via.types

/** Represents the route param. Eg: /api/customer/{id}
  */
sealed trait Param:
  val name: String

/** Router int param
  * @param name
  *   Param name
  * @param value
  *   Param value
  */
case class ParamInt(override val name: String, value: Int) extends Param

/** Router long param
  * @param name
  *   Param name
  * @param value
  *   Param value
  */
case class ParamLong(override val name: String, value: Long) extends Param

/** Router string param
  * @param name
  *   Param name
  * @param value
  *   Param value
  */
case class ParamStr(override val name: String, value: String) extends Param

/** Router tail paths param
  * @param name
  *   Param name
  * @param tail
  *   The tail path
  */
case class ParamPaths(override val name: String, tail: List[String])
    extends Param

/** Params chain
  * @param raw
  *   The params
  */
case class Params(raw: Seq[Param] = Nil):
  /** Int param
    * @param name
    *   Param name
    * @return
    *   The param
    */
  def int(name: String): Option[Int] = raw.find(_.name == name) match
    case Some(ParamInt(_, i)) => Some(i)
    case _                    => None

  /** Long param
    * @param name
    *   Param name
    * @return
    *   The param
    */
  def long(name: String): Option[Long] = raw.find(_.name == name) match
    case Some(ParamLong(_, i)) => Some(i)
    case _                     => None

  /** String param
    *
    * @param name
    *   Param name
    * @return
    *   The param
    */
  def str(name: String): Option[String] = raw.find(_.name == name) match
    case Some(ParamStr(_, i)) => Some(i)
    case _                    => None

  /** Paths params (tail)
    *
    * @param name
    *   Param name
    * @return
    *   The paths tail param
    */
  def paths(name: String): List[String] = raw.find(_.name == name) match
    case Some(ParamPaths(_, i)) => i
    case _                      => Nil

  /** Paths params (tail)
    *
    * @param name
    *   Param name
    * @return
    *   The paths tail param
    */
  def tail(name: String): List[String] = paths(name)

  /** @return
    *   The params size
    */
  def size: Int = raw.size

  /** @return
    *   The params indexes
    */
  def indices: Range = raw.indices

  /** Get param by index
    * @param i
    *   Param index
    * @return
    *   The param at index position
    */
  def at(i: Int): Param = raw(i)
