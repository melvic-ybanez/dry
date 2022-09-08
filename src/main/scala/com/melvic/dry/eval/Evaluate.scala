package com.melvic.dry.eval

import com.melvic.dry.result.Result.Result
import com.melvic.dry.Value
import com.melvic.dry.Value.{Bool, Str, None => VNone}

object Evaluate extends EvalExpr with EvalDecl {
  type Evaluate[A] = A => Result[Value]

  def isTruthy(value: Value): Boolean =
    value match {
      case VNone       => false
      case Str("")     => false
      case Bool(value) => value
      case _           => true
    }
}
