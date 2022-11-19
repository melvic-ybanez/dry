package com.melvic.dry.interpreter.values

import com.melvic.dry.Show
import com.melvic.dry.ast.Decl.Def
import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.aux.implicits.ListOps
import com.melvic.dry.interpreter.values.Value.Num
import com.melvic.dry.result.Failure

private[interpreter] trait Value {
  def toNum: Option[Num] =
    this match {
      case num: Num => Some(num)
      case _        => None
    }
}

object Value {
  type None = None.type
  case object None extends Value

  final case class Bool(value: Boolean) extends Value
  final case class Num(value: Double) extends Value
  final case class Str(value: String) extends Value

  case object Unit extends Value

  final case class Returned(value: Value) extends Value

  final case class RuntimeError(failure: Failure.RuntimeError) extends Value

  def show: Show[Value] = {
    case None        => "none"
    case Bool(value) => value.toString
    case Num(value) =>
      val str = value.toString
      if (str.endsWith(".0")) str.init.init
      else str
    case Str(str)                                 => str
    case Value.Unit                               => ""
    case Callable.Function(Def(name, _, _), _, _) => show"<function $name>"
    case Callable.Lambda(_, _)                    => s"<lambda function>"
    case DClass(name, _, _)                       => name
    case _: Callable                              => "<callable>"
    case DInstance(klass, _)                      => show"$klass instance"
    case DList(elems, _)                          => show"[${elems.map(Value.show).toCsv}]"
    case RuntimeError(error)                      => Failure.show(error)
  }

  implicit class ToValue[A](value: A) {
    def unit: Value.Unit.type = {
      value
      Value.Unit
    }
  }
}
