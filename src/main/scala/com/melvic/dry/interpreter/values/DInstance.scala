package com.melvic.dry.interpreter.values

import com.melvic.dry.Token
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.Result

import scala.collection.mutable

final case class DInstance private (klass: DClass, fields: mutable.Map[String, Value]) extends Value {
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

object DInstance {
  def fromClass(klass: DClass): DInstance = DInstance(klass, mutable.Map.empty)
}
