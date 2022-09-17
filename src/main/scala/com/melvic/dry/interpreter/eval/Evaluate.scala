package com.melvic.dry.interpreter.eval

object Evaluate extends EvalExpr with EvalDecl {
  type Evaluate[A] = A => EvalResult
}
