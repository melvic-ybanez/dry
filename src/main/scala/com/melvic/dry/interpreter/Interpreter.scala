package com.melvic.dry.interpreter

import com.melvic.dry.ast.Decl
import com.melvic.dry.interpreter.Env.LocalEnv
import com.melvic.dry.interpreter.Keys.{SuccessCount, TestCount}
import com.melvic.dry.interpreter.eval.Evaluate.Out
import com.melvic.dry.interpreter.eval.{Context, Evaluate}
import com.melvic.dry.interpreter.values.Callable.Varargs
import com.melvic.dry.interpreter.values.Value.{Num, Str, ToValue, Types}
import com.melvic.dry.interpreter.values._
import com.melvic.dry.resolver.Locals
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.implicits.ToResult

import java.nio.file.Path
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine

object Interpreter {
  def interpret(declarations: List[Decl], enclosing: Env, locals: Locals, sourcePaths: List[Path]): Out = {
    val env = LocalEnv(enclosing.table, natives)
    def recurse(declarations: List[Decl], value: Value): Out =
      declarations match {
        case Nil => Result.succeed(value)
        case statement :: rest =>
          Evaluate
            .decl(Context(statement, env, locals, sourcePaths))
            .flatMap(recurse(rest, _))
      }

    recurse(declarations, Value.Unit)
  }

  val natives: Env = Env.empty
    .defineWith("print", Callable.unarySuccess(_)(arg => print(Value.show(arg)).unit))
    // we don't have standard library functions yet, so we are building a dedicated function for println for now.
    // Once, user-defined functions are supported, we can just replace this with a call to `print`, applied
    // to a string that ends in a newline character
    .defineWith("println", Callable.unarySuccess(_)(arg => println(Value.show(arg)).unit))
    .defineWith(
      "readLine",
      Callable.withLineNo(1, _)(line => {
        case Str(prompt) :: _ => Str(readLine(prompt)).ok
        case arg :: _         => RuntimeError.invalidArgument(Types.String, Value.typeOf(arg), line).fail
        case Nil              => RuntimeError.invalidArgument(Types.String, Types.None, line).fail
      })
    )
    .defineWith("str", Callable.unarySuccess(_)(arg => Str(Value.show(arg))))
    .defineWith("typeof", typeOf)
    .define(TestCount, Num(0))
    .define(SuccessCount, Num(0))
    .defineWith("assert_equal", Assertions.assertEqual)
    .defineWith("assert_error", Assertions.assertError)
    .defineWith("show_test_results", Assertions.showTestResults)
    .defineWith("list", env => Varargs(env, elems => DList(elems.to(ListBuffer), env).ok))
    .defineWith("Errors", errors)

  private def typeOf: Env => Callable = Callable.unarySuccess(_)(value => Str(Value.typeOf(value)))

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
