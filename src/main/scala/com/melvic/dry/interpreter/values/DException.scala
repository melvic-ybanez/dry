package com.melvic.dry.interpreter.values

import com.melvic.dry.Token
import com.melvic.dry.interpreter.Env
import com.melvic.dry.interpreter.values.DException.Attributes
import com.melvic.dry.interpreter.values.Value.{Types, typeOf}
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Failure.RuntimeError.Kind
import com.melvic.dry.result.Result.implicits.ToResult

class DException(val kind: Kind, val env: Env) extends DClass(kind.exceptionName, Map.empty, env) {
  addKindField(this)

  override def arity = 1

  // TODO: Add support for the other exception constructors. This would
  //  make it more convenient to compare exception objects (no more direct message comparison)
  override def call(token: Token) = {
    case args @ ((message: Value.Str) :: _) =>
      super.call(token)(args).flatMap { case instance: DInstance =>
        addKindField(instance)
          .addField(Attributes.Message, message)
          .addField(Attributes.Line, Value.Num(token.line))
          .ok
      }
    case arg :: _ => RuntimeError.invalidArgument(s"${Types.String}", typeOf(arg), token.line).fail
  }

  private def addKindField(dObject: DObject): DObject =
    dObject.addField(Attributes.Kind, Value.Str(kind.exceptionName))
}

object DException {
  object Attributes {
    val Message: String = "__message__"
    val Kind: String = "__kind__"
    val Line: String = "__line__"
  }

  def apply(kind: Kind, env: Env): DException =
    new DException(kind, env)

  def unapply(exception: DException): Option[(Kind, Env)] =
    Some(exception.kind, exception.env)

  def kindOf(instance: DInstance): Option[String] =
    asString(instance.getField(Attributes.Kind))

  def messageOf(instance: DInstance): Option[String] =
    asString(instance.getField(Attributes.Message))

  def lineOf(instance: DInstance): Option[Int] =
    instance.getField(Attributes.Line).flatMap(_.toNum).map(_.value.toInt)

  private def asString(value: Option[Value]): Option[String] =
    value.flatMap {
      case Value.Str(value) => Some(value)
      case _                => None
    }
}
