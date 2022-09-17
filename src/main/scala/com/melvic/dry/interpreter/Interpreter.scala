package com.melvic.dry.interpreter

import com.melvic.dry.ast.Decl
import com.melvic.dry.interpreter.Env.LocalEnv
import com.melvic.dry.interpreter.Value.ToValue
import com.melvic.dry.interpreter.eval.{EvalOut, Evaluate}
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.implicits.ToResult

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

    recurse(declarations, LocalEnv(env.table, natives), Value.Unit)
  }

  def natives: Env = Env.empty
    .define("print", Callable(1, { case arg :: _ => print(Value.show(arg)).unit.env }))
    // we don't have standard library functions yet, so we are building a dedicated function for println for now.
    // Once, user-defined functions are supported, we can just replace this with a call to `print`, applied
    // to a string that ends in a newline character
    .define("println", Callable(1, { case arg :: _ => println(Value.show(arg)).unit.env }))
}
