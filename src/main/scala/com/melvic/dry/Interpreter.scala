package com.melvic.dry

import com.melvic.dry.Result.Result
import com.melvic.dry.Result.impilcits.ToResult
import com.melvic.dry.ast.Decl
import com.melvic.dry.eval.Evaluate

object Interpreter {
  def interpret(statements: List[Decl]): Result[Unit] = {
    def recurse(statements: List[Decl]): Result[Unit] =
      statements match {
        case Nil => ().ok
        case statement :: rest =>
          Evaluate.decl(statement).map(_ => recurse(rest))
      }

    recurse(statements)
  }
}
