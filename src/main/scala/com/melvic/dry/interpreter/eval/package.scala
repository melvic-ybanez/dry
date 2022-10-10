package com.melvic.dry.interpreter

import com.melvic.dry.interpreter.Value.{Bool, Str, None => VNone}
import com.melvic.dry.interpreter.values.Value
import com.melvic.dry.resolver.Locals
import com.melvic.dry.result.Result.Result

package object eval {
  type Evaluate[A] = Evaluate.Evaluate[A]
  type EvalResult = EvalResultF[Value]
  type EvalResultF[+V] = Env => Result[V]
  type EvalOut = Result[Value]

  def isTruthy(value: Value): Boolean =
    value match {
      case VNone       => false
      case Str("")     => false
      case Bool(value) => value
      case _           => true
    }
}
