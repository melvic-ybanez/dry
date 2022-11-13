package com.melvic.dry.interpreter

import com.melvic.dry.ast.Decl
import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.interpreter.Env.LocalEnv
import com.melvic.dry.interpreter.eval.{EvalOut, Evaluate}
import com.melvic.dry.interpreter.values.Callable
import com.melvic.dry.interpreter.values.Value.{Bool, Num, Str, ToValue}
import com.melvic.dry.resolver.Locals
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.implicits.ToResult

object Interpreter {
  def interpret(declarations: List[Decl], enclosing: Env, locals: Locals): EvalOut = {
    val env = LocalEnv(enclosing.table, natives.withLocals(locals))
    def recurse(declarations: List[Decl], value: Value): EvalOut =
      declarations match {
        case Nil => Result.succeed(value)
        case statement :: rest =>
          Evaluate
            .decl(statement)
            .andThen(_.flatMap(recurse(rest, _)))(env)
      }

    recurse(declarations, Value.Unit)
  }

  def natives: Env = Env.empty
    .defineWith("print", Callable.unarySuccess(_)(arg => print(Value.show(arg)).unit))
    // we don't have standard library functions yet, so we are building a dedicated function for println for now.
    // Once, user-defined functions are supported, we can just replace this with a call to `print`, applied
    // to a string that ends in a newline character
    .defineWith("println", Callable.unarySuccess(_)(arg => println(Value.show(arg)).unit))
    .defineWith("str", Callable.unarySuccess(_)(arg => Str(Value.show(arg))))
    .defineWith(
      "typeof",
      Callable.unarySuccess(_) {
        case Value.None  => Str("none")
        case Bool(_)     => Str("boolean")
        case Num(_)      => Str("number")
        case Str(_)      => Str("string")
        case Value.Unit  => Str("unit")
        case _: Callable => Str("function")
      }
    )
    .defineWith(
      "assert",
      Callable(3, _) { case arg1 :: arg2 :: message :: _ =>
        if (arg1 == arg2) println(show"${Console.GREEN}[Successful] $message")
        else System.err.println(show"[Failure] Expected: $arg1. Got: $arg2")
        Value.unit.ok
      }
    )
}
