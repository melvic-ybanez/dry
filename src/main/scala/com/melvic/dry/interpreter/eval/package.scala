package com.melvic.dry.interpreter

import com.melvic.dry.interpreter.Value.{Bool, Str, None => VNone}
import com.melvic.dry.result.Result.Result

package object eval {
  type Evaluate[A] = Evaluate.Evaluate[A]
  type EvalResult = EvalResultF[Value]
  type EvalResultF[+V] = Env => EvalOutF[V]
  type EvalOut = EvalOutF[Value]
  type EvalOutF[+V] = Result[(V, Env)]

  def toEvalResult(f: Env => Result[Value]): EvalResult =
    env => f(env).map((_, env))

  def isTruthy(value: Value): Boolean =
    value match {
      case VNone       => false
      case Str("")     => false
      case Bool(value) => value
      case _           => true
    }
}
