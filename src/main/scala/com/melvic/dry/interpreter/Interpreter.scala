package com.melvic.dry.interpreter

import com.melvic.dry.ast.Decl
import com.melvic.dry.eval.Evaluate
import com.melvic.dry.result.Result.Result
import com.melvic.dry.result.Result.implicits.ToResult

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
