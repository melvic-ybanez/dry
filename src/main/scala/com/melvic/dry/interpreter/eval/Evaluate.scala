package com.melvic.dry.interpreter.eval

import com.melvic.dry.interpreter.values.Value
import com.melvic.dry.result.Result.Result

object Evaluate extends EvalExpr with EvalDecl {
  type Out = Result[Value]
}
