package com.melvic.dry.interpreter

import com.melvic.dry.ast.Decl
import com.melvic.dry.interpreter.Env.LocalEnv
import com.melvic.dry.interpreter.Value.{Bool, Num, Str, ToValue}
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
    .define("print", Callable.unary(arg => print(Value.show(arg)).unit.env))
    // we don't have standard library functions yet, so we are building a dedicated function for println for now.
    // Once, user-defined functions are supported, we can just replace this with a call to `print`, applied
    // to a string that ends in a newline character
    .define("println", Callable.unary(arg => println(Value.show(arg)).unit.env))
    .define("str", Callable.unary(arg => Str(Value.show(arg)).env))
    .define(
      "typeof", {
        def str(str: String) = Str(str).env

        Callable.unary {
          case Value.None  => str("none")
          case Bool(_)     => str("boolean")
          case Num(_)      => str("number")
          case Str(_)      => str("string")
          case Value.Unit  => str("unit")
          case _: Callable => str("function")
        }
      }
    )
}
