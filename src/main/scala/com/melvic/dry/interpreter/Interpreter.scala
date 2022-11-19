package com.melvic.dry.interpreter

import com.melvic.dry.ast.Decl
import com.melvic.dry.interpreter.Env.LocalEnv
import com.melvic.dry.interpreter.Keys.{SuccessCount, TestCount}
import com.melvic.dry.interpreter.eval.{EvalOut, Evaluate}
import com.melvic.dry.interpreter.values.Callable.Varargs
import com.melvic.dry.interpreter.values.Value.{Bool, Num, Str, ToValue}
import com.melvic.dry.interpreter.values._
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

  val natives: Env = Env.empty
    .defineWith("print", Callable.unarySuccess(_)(arg => print(Value.show(arg)).unit))
    // we don't have standard library functions yet, so we are building a dedicated function for println for now.
    // Once, user-defined functions are supported, we can just replace this with a call to `print`, applied
    // to a string that ends in a newline character
    .defineWith("println", Callable.unarySuccess(_)(arg => println(Value.show(arg)).unit))
    .defineWith("str", Callable.unarySuccess(_)(arg => Str(Value.show(arg))))
    .defineWith("typeof", typeOf)
    .define(TestCount, Num(0))
    .define(SuccessCount, Num(0))
    .defineWith("assert", Assertions.assert)
    .defineWith("assert_error", Assertions.assertError)
    .defineWith("show_test_results", Assertions.showTestResults)
    .defineWith("list", env => Varargs(env, elems => DList(elems, env).ok))
    .defineWith("Errors", errors)

  private def typeOf: Env => Callable = Callable.unarySuccess(_) {
    case Value.None   => Str("none")
    case Bool(_)      => Str("boolean")
    case Num(_)       => Str("number")
    case Str(_)       => Str("string")
    case Value.Unit   => Str("unit")
    case _: DClass    => Str("class")
    case _: DInstance => Str("instance")
    case _: DList     => Str("list")
    case _: DObject   => Str("object")
    case _: Callable  => Str("callable")
  }

  private def errors: Env => DClass = DClass("Errors", Map.empty, _)
    .addField("DIVISION_BY_ZERO", Str(Keys.Errors.DivisionByZero))
    .addField("INVALID_OPERAND", Str(Keys.Errors.InvalidOperand))
    .addField("INVALID_OPERANDS", Str(Keys.Errors.InvalidOperands))
    .addField("UNDEFINED_VARIABLE", Str(Keys.Errors.UndefinedVariable))
    .addField("NOT_CALLABLE", Str(Keys.Errors.NotCallable))
    .addField("INCORRECT_ARITY", Str(Keys.Errors.IncorrectArity))
    .addField("DOES_NOT_HAVE_PROPERTIES", Str(Keys.Errors.DoesNotHaveProperties))
    .addField("UNDEFINED_PROPERTY", Str(Keys.Errors.UndefinedProperty))
    .addField("INDEX_OUT_OF_BOUNDS", Str(Keys.Errors.IndexOutOfBounds))
    .addField("INVALID_INDEX", Str(Keys.Errors.InvalidIndex))
}
