package com.melvic.dry.interpreter.values

import com.melvic.dry.Token
import com.melvic.dry.interpreter.Env
import com.melvic.dry.interpreter.eval.Evaluate.Out
import com.melvic.dry.interpreter.values.DException.{Fields, addFields}
import com.melvic.dry.interpreter.values.Value.{Types, typeOf}
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Failure.RuntimeError.Kind
import com.melvic.dry.result.Result.implicits.ToResult

class DException(val kind: Kind, val env: Env) extends DClass(kind.exceptionName, Map.empty, env) {
  override def arity = 1

  override def call(token: Token) = {
    case args @ ((message: Value.Str) :: _) =>
      super.call(token)(args).flatMap { case instance: DInstance =>
        instance
          .addField(Fields.Kind, Value.Str(kind.exceptionName))
          .addField(Fields.Message, message)
          .addField(Fields.Line, Value.Num(token.line))
          .ok
      }
    case arg :: _ => RuntimeError.invalidArgument(s"${Types.String}", typeOf(arg), token.line).fail
  }
}

object DException {
  class NoArgDException(override val kind: Kind, override val env: Env) extends DException(kind, env) {
    override def arity = 0

    override def call(token: Token) = {
      case Nil => super.call(token)(Value.Str(kind.exceptionName) :: Nil)
      case _   => RuntimeError.invalidArgument(s"${Types.String}", typeOf(Value.Unit), token.line).fail
    }
  }

  object NoArgDException {
    def apply(kind: Kind, env: Env): NoArgDException =
      new NoArgDException(kind, env)
  }

  object Fields {
    val Message: String = "message"
    val Kind: String = "kind"
    val Line: String = "line"
  }

  def apply(kind: Kind, env: Env): DException =
    new DException(kind, env)

  def unapply(exception: DException): Option[(Kind, Env)] =
    Some(exception.kind, exception.env)

  def kindOf(instance: DInstance): Option[String] =
    asString(instance.getField(Fields.Kind))

  def messageOf(instance: DInstance): Option[String] =
    asString(instance.getField(Fields.Message))

  def lineOf(instance: DInstance): Option[Int] =
    instance.getField(Fields.Line).flatMap(_.toNum).map(_.value.toInt)

  private def asString(value: Option[Value]): Option[String] =
    value.flatMap {
      case Value.Str(value) => Some(value)
      case _                => None
    }

  private def addFields(instance: DInstance, kind: Kind, message: String, token: Token): Out =
    instance
      .addField(Fields.Kind, Value.Str(kind.exceptionName))
      .addField(Fields.Message, Value.Str(message))
      .addField(Fields.Line, Value.Num(token.line))
      .ok
}
