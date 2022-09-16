package com.melvic.dry

import com.melvic.dry.Value.Num

import scala.annotation.tailrec

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

  sealed trait Unit extends Value

  final case class ExprStmt(value: Value) extends Unit
  case object Unit extends Unit

  final case class Callable(arity: Int, call: List[Value] => Value) extends Value

  @tailrec
  def show(value: Value): String =
    value match {
      case None        => "None"
      case Bool(value) => value.toString
      case Num(value) =>
        val str = value.toString
        if (str.endsWith(".0")) str.init.init
        else str
      case Str(str)                     => str
      case Value.Unit                   => ""
      case Value.ExprStmt(value: Value) => Value.show(value)
    }

  implicit class ToValue[A](value: A) {
    def unit: Value.Unit = {
      value
      Value.Unit
    }
  }
}
