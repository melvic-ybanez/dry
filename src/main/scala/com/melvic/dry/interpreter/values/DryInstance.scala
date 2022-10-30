package com.melvic.dry.interpreter.values

import com.melvic.dry.Token
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.Result

final case class DryInstance private (klass: DryClass, fields: Map[String, Value]) extends Value {
  def get(name: Token): Result[Value] =
    Result.fromOption(fields.get(name.lexeme), RuntimeError.undefinedProperty(name))
}

object DryInstance {
  def fromClass(klass: DryClass): DryInstance = DryInstance(klass, Map.empty)
}
