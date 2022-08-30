package com.melvic.dry

import scala.language.implicitConversions

sealed trait Value {
  def toNum: Value.Num =
    this match {
      case num: Value.Num => num
      case _ => throw new ClassCastException("value to number")
    }
}

object Value {
  case object None extends Value

  final case class Bool(value: Boolean) extends Value
  final case class Num(value: Double) extends Value
  final case class Str(value: String) extends Value

  object implicits {
    implicit def valueToDouble(value: Value): Double =
      value.toNum

    implicit def numToDouble(num: Num): Double =
      num.value

    implicit def doubleToNum(value: Double): Num =
      Num(value)
  }
}
