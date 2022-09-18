package com.melvic.dry.interpreter

import com.melvic.dry.ast.Decl.Def
import com.melvic.dry.interpreter.Value.Num

private[interpreter] trait Value {
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

  final case class Returned(value: Value) extends Value

  def show(value: Value): String =
    value match {
      case None        => "none"
      case Bool(value) => value.toString
      case Num(value) =>
        val str = value.toString
        if (str.endsWith(".0")) str.init.init
        else str
      case Str(str)                              => str
      case Value.Unit                            => ""
      case Callable.Function(Def(name, _, _), _) => s"<function $name>"
      case Callable.Lambda(_, _)                 => s"<lambda function>"
      case _: Callable                           => "<callable>"
    }

  implicit class ToValue[A](value: A) {
    def unit: Value.Unit.type = {
      value
      Value.Unit
    }
  }
}
