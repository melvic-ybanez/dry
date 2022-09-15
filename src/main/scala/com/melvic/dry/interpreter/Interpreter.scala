package com.melvic.dry.interpreter

import com.melvic.dry.ast.Decl
import com.melvic.dry.eval.{EvalOut, Evaluate}
import com.melvic.dry.implicits._
import com.melvic.dry.result.Result
import com.melvic.dry.{Env, Value}

object Interpreter {
  def interpret(declarations: List[Decl], env: Env): EvalOut = {
    def recurse(declarations: List[Decl], env: Env, value: Value): EvalOut =
      declarations match {
        case Nil => Result.succeed(value, env)
        case statement :: rest =>
          Evaluate
            .decl(statement)
            .andThen(_.flatMap { case (value, env) =>
              recurse(rest, env, value)
            })(env)
      }

    recurse(declarations, env, Value.Unit)
  }
}
