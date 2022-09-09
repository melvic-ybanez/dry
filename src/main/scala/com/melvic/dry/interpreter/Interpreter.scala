package com.melvic.dry.interpreter

import com.melvic.dry.Env
import com.melvic.dry.ast.Decl
import com.melvic.dry.eval.Evaluate
import com.melvic.dry.result.Result.Result
import com.melvic.dry.result.Result.implicits.ToResult
import com.melvic.dry.implicits._

object Interpreter {
  def interpret(declarations: List[Decl]): Result[Unit] = {
    def recurse(declarations: List[Decl], env: Env): Result[Unit] =
      declarations match {
        case Nil => ().ok
        case statement :: rest =>
          Evaluate.decl(statement).map(_.flatMap { case (_, env) =>
            recurse(rest, env)
          })(env)
      }

    recurse(declarations, Env.empty)
  }
}
