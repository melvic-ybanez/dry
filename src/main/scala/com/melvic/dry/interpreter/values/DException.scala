package com.melvic.dry.interpreter.values

import com.melvic.dry.Token
import com.melvic.dry.interpreter.Env
import com.melvic.dry.interpreter.values.DException.Kind
import com.melvic.dry.interpreter.values.Value.{Types, typeOf}
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result.implicits.ToResult

class DException(val kind: Kind, val env: Env) extends DClass(kind.name, Map.empty, env) {
  override def arity = 1

  override def call(token: Token) = {
    case args @ ((message: Value.Str) :: _) =>
      super.call(token)(args).flatMap { case instance: DInstance =>
        instance.addField("exception_type", Value.Str(kind.name)).addField("message", message).ok
      }
    case arg :: _ => RuntimeError.invalidArgument(s"${Types.String}", typeOf(arg), token.line).fail
  }
}

object DException {
  sealed trait Kind {
    val name: String = this.toString
  }

  object Kind {
    def of(instance: DInstance): Option[String] =
      asString(instance.fields.get("exception_type"))
  }

  case object DivisionByZero extends Kind
  case object UndefinedVariable extends Kind
  case object InvalidOperand extends Kind

  def apply(kind: Kind, env: Env): DException =
    new DException(kind, env)

  def unapply(exception: DException): Option[(Kind, Env)] =
    Some(exception.kind, exception.env)

  def messageOf(exception: DInstance): Option[String] =
    asString(exception.fields.get("message"))

  private def asString(value: Option[Value]): Option[String] =
    value.flatMap {
      case Value.Str(value) => Some(value)
      case _                => None
    }
}
