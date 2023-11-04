package com.melvic.dry.interpreter.eval

import com.melvic.dry.Token
import com.melvic.dry.ast.Expr
import com.melvic.dry.interpreter.values.Value
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result.Result

object Evaluate extends EvalExpr with EvalDecl {
  type Out = Result[Value]

  private[eval] def accessNumericIndex(evaluatedKey: Value, key: Expr, limit: Int, token: Token)(
      access: Int => Out
  ): Out =
    evaluatedKey match {
      case Value.Num(index) if index % 1 == 0 =>
        val intIndex = index.toInt
        if (index < 0 || index >= limit)
          RuntimeError.indexOutOfBounds(intIndex, token.line).fail[Value]
        else access(intIndex)
      case _ => RuntimeError.invalidIndex(key, token).fail[Value]
    }
}
