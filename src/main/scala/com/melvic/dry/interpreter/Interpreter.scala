package com.melvic.dry.interpreter

import com.melvic.dry.ast.Decl
import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.interpreter.Env.LocalEnv
import com.melvic.dry.interpreter.eval.{EvalOut, Evaluate}
import com.melvic.dry.interpreter.values.Value.{Bool, Num, Str, ToValue}
import com.melvic.dry.interpreter.values.{Callable, DClass, DInstance, DList}
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
    .defineWith("typeof", typeOf)
    .defineWith("assert", assert)
    .defineWith("list", list)

  private def typeOf: Env => Callable = Callable.unarySuccess(_) {
    case Value.None   => Str("none")
    case Bool(_)      => Str("boolean")
    case Num(_)       => Str("number")
    case Str(_)       => Str("string")
    case Value.Unit   => Str("unit")
    case _: DClass    => Str("class")
    case _: DInstance => Str("instance")
    case _: Callable  => Str("function")
  }

  private def assert: Env => Callable = Callable(3, _) { case description :: value1 :: value2 :: _ =>
    if (value1 == value2) println(show"${Console.GREEN}[Success] $description${Console.RESET}")
    else System.err.println(show"[Failure] $description. Expected: $value1. Got: $value2")
    Value.unit.ok
  }

  private def list(env: Env): Callable = Callable.varargs(env)(elems => DList(elems, env).ok)
}
