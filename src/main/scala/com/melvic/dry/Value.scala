package com.melvic.dry

import com.melvic.dry.Value.Num

sealed trait Value {
  def toNum: Option[Num] =
    this match {
      case num: Num => Some(num)
      case _        => None
    }
}

object Value {
  case object None extends Value

  final case class Bool(value: Boolean) extends Value
  final case class Num(value: Double) extends Value
  final case class Str(value: String) extends Value
  case object Unit extends Value

  def show(value: Value): String =
    value match {
      case None => "None"
      case Bool(value) => value.toString
      case Num(value) =>
        val str = value.toString
        if (str.endsWith(".0")) str.init.init
        else str
      case Str(str) => str
      case Value.Unit => ""
    }
}
