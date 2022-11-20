package com.melvic.dry.interpreter.values

import com.melvic.dry.Show
import com.melvic.dry.ast.Decl.Def
import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.aux.implicits.ListOps
import com.melvic.dry.interpreter.Callable
import com.melvic.dry.interpreter.values.Value.Num

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

  def typeOf: Value => String = {
    case Value.None   => "none"
    case Bool(_)      => "boolean"
    case Num(_)       => "number"
    case Str(_)       => "string"
    case Value.Unit   => "unit"
    case _: DClass    => "class"
    case _: DInstance => "instance"
    case _: DList     => "list"
    case _: DObject   => "object"
    case _: Callable  => "callable"
  }

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
    case DList(elems, _)                          => show"[${elems.toList.map(Value.show).toCsv}]"
  }

  implicit class ToValue[A](value: A) {
    def unit: Value.Unit.type = {
      value
      Value.Unit
    }
  }
}
