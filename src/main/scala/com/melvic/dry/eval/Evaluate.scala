package com.melvic.dry.eval

object Evaluate extends EvalExpr with EvalDecl {
  type Evaluate[A] = A => EvalResult
}
