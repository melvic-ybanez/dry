package com.melvic.dry.interpreter.values

import com.melvic.dry.Token
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.Result

import scala.collection.mutable

final case class DryInstance private (klass: DryClass, fields: mutable.Map[String, Value]) extends Value {
  def get(name: Token): Result[Value] =
    Result.fromOption(
      fields.get(name.lexeme).orElse(klass.findMethod(name.lexeme).map(_.bind(this))),
      RuntimeError.undefinedProperty(name)
    )

  def set(name: Token, value: Value): Value.None = {
    fields += (name.lexeme -> value)
    Value.None
  }
}

object DryInstance {
  def fromClass(klass: DryClass): DryInstance = DryInstance(klass, mutable.Map.empty)
}
