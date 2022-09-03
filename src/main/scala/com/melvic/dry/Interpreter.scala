package com.melvic.dry

import com.melvic.dry.Result.Result
import com.melvic.dry.Result.impilcits.ToResult
import com.melvic.dry.ast.{Expr, Stmt}
import com.melvic.dry.eval.Evaluate

object Interpreter {
  def interpret(statements: List[Stmt]): Result[Unit] = {
    def recurse(statements: List[Stmt]): Result[Unit] =
      statements match {
        case Nil => ().ok
        case statement :: rest =>
          Evaluate.stmt(statement).map(_ => recurse(rest))
      }

    recurse(statements)
  }
}
