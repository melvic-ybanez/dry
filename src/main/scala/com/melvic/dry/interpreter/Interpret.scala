package com.melvic.dry.interpreter

import com.melvic.dry.ast.Decl
import com.melvic.dry.interpreter.Env.LocalEnv
import com.melvic.dry.interpreter.natives.Keys.{Errors, SuccessCount, TestCount}
import com.melvic.dry.interpreter.eval.Evaluate.Out
import com.melvic.dry.interpreter.eval.{Context, Evaluate}
import com.melvic.dry.interpreter.natives.{Assertions, Exceptions}
import com.melvic.dry.interpreter.values.Value.{Num, Str, ToValue, Types}
import com.melvic.dry.interpreter.values._
import com.melvic.dry.lexer.Lexer
import com.melvic.dry.parsers.Parser
import com.melvic.dry.resolver
import com.melvic.dry.resolver.{Locals, Resolve, Scopes}
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.Result
import com.melvic.dry.result.Result.implicits.ToResult

import java.nio.file.Path
import scala.io.StdIn.readLine
import scala.util.chaining.scalaUtilChainingOps

object Interpret {
  def declarations(declarations: List[Decl], enclosing: Env, locals: Locals, sourcePaths: List[Path]): Out = {
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

  // noinspection SpellCheckingInspection
  def script(
      source: String,
      env: Env,
      scopes: Scopes,
      sourcePaths: List[Path]
  ): Result[(Value, Scopes)] =
    for {
      tokens  <- Lexer.scanTokens(source)
      decls   <- Parser.fromTokens(tokens).parse.result
      context <- Resolve.resolveAll(decls)(resolver.Context.default.copy(scopes = scopes))
      value   <- Interpret.declarations(decls, env, context.locals, sourcePaths)
    } yield (value, context.scopes)

  def expression(source: String, env: Env, scopes: Scopes): Result[Value] =
    for {
      tokens  <- Lexer.scanTokens(source)
      expr    <- Parser.fromTokens(tokens).expression.result
      context <- Resolve.expr(expr)(resolver.Context.default.copy(scopes = scopes))
      value   <- Evaluate.expr(Context(expr, env, context.locals, Nil))
    } yield value

  val natives: Env = Env.empty
    .defineWith("print", Callable.unarySuccess(_)(arg => print(Value.show(arg)).unit))
    // TODO: Add this to stdlib or prelude, defined in terms of `print`
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
    .defineWith("Errors", errors)
    .pipe(Assertions.register)
    .pipe(Exceptions.register)

  private def typeOf: Env => Callable = Callable.unarySuccess(_)(value => Str(Value.typeOf(value)))

  private def errors(env: Env): DClass =
    Errors.allErrors.foldLeft(DClass.default("Errors", env)) { (dClass, error) =>
      val fieldName = error.drop(2).dropRight(2).toUpperCase
      dClass.addField(fieldName, Str(error))
    }
}
