package com.melvic.dry.interpreter.values

import com.melvic.dry.Show
import com.melvic.dry.ast.Decl.Def
import com.melvic.dry.aux.Show
import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.aux.implicits.ListOps
import com.melvic.dry.interpreter.Callable
import com.melvic.dry.interpreter.values.Value.Num

import scala.util.chaining.scalaUtilChainingOps

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

  object Types {
    val String = "string"
    val None = "none"
    val Boolean = "bool"
    val Number = "number"
    val Unit = "unit"
    val Class = "class"
    val Instance = "instance"
    val List = "list"
    val Object = "object"
    val Dictionary = "dictionary"
    val Callable = "callable"
    val Tuple = "tuple"
    val Exception = "exception"
  }

  def typeOf: Value => String = {
    case Value.None     => Types.None
    case Bool(_)        => Types.Boolean
    case Num(_)         => Types.Number
    case Str(_)         => Types.String
    case Value.Unit     => Types.Unit
    case _: DException  => Types.Exception
    case _: DClass      => Types.Class
    case _: DInstance   => Types.Instance
    case _: DList       => Types.List
    case _: DTuple      => Types.Tuple
    case _: DDictionary => Types.Dictionary
    case _: DObject     => Types.Object
    case _: Callable    => Types.Callable
  }

  implicit def implicitShow: Show[Value] = show

  def show: Show[Value] = {
    case None        => "none"
    case Bool(value) => value.toString
    case Num(value) =>
      val str = value.toString
      if (str.endsWith(".0")) str.init.init
      else str
    case Str(str)                                       => str
    case Value.Unit                                     => "()"
    case Callable.Function(Def(name, _, _), _, _, _, _) => show"<function $name>"
    case Callable.Lambda(_, _, _, _)                    => s"<lambda function>"
    case DClass(name, _, _)                             => name
    case _: Callable                                    => "<callable>"
    case DInstance(klass, _)                            => show"$klass instance"
    case DList(elems, _)                                => show"[${Show.list(elems.toList)}]"
    case DTuple(elem :: Nil, _)                         => show"($elem,)"
    case DTuple(elems, _)                               => show"(${Show.list(elems)})"
    case DDictionary(table, _) =>
      show"{${table.toList.map { case (key, value) => show"$key: $value" }.toCsv}}"
  }

  implicit class ToValue[A](value: A) {
    def unit: Value.Unit.type = {
      value.pipe(_ => Value.Unit)
    }
  }
}
