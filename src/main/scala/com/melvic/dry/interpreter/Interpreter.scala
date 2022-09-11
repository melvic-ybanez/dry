package com.melvic.dry.interpreter

import com.melvic.dry.ast.Decl
import com.melvic.dry.eval.Evaluate
import com.melvic.dry.implicits._
import com.melvic.dry.result.Result.Result
import com.melvic.dry.result.Result.implicits.ToResult
import com.melvic.dry.{Env, Value}

object Interpreter {
  def interpret(declarations: List[Decl]): Result[Value] = {
    def recurse(declarations: List[Decl], env: Env, value: Value): Result[Value] =
      declarations match {
        case Nil => value.ok
        case statement :: rest =>
          Evaluate
            .decl(statement)
            .map(_.flatMap { case (value, env) =>
              recurse(rest, env, value)
            })(env)
      }

    recurse(declarations, Env.empty, Value.Unit)
  }
}
