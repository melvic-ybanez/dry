package com.melvic.dry.interpreter

import com.melvic.dry.lexer.Lexer
import com.melvic.dry.parsers.Parser
import com.melvic.dry.resolver.{Context, Locals, Resolve}
import com.melvic.dry.result.Result.Result
import com.melvic.dry.result.{Failure, Result}

import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.chaining.scalaUtilChainingOps

object Run {
  def repl(env: Env, locals: Locals): Unit = {
    val input = readLine("> ")
    if (input == "exit") ()
    else {
      Run
        .source(
          ".",
          // this is a trick I'm using to make repl accept expressions that
          // do not end with semicolons.
          if (!input.endsWith(";") && !input.endsWith("}")) input + ";"
          else input,
          env,
          locals
        )
        .map { case (value, locals) =>
          if (value != Value.Unit)
            println(Value.show(value))
          locals
        }
        .pipe { result =>
          Result.foreachFailure(result)(error => System.err.println(Failure.show(error)))
          result match {
            case Left(_)          => repl(env, locals)
            case Right(newLocals) => repl(env, newLocals)
          }
        }
    }
  }

  def mainModule(path: String): Unit = {
    val result = Run.path(path, path)
    Result.foreachFailure(result)(error => System.err.println(Failure.show(error)))
    System.exit(if (result.isLeft) -1 else 0)
  }

  def path(mainModule: String, path: String): Result[Env] = {
    val env = Env.empty

    val source = Source.fromFile(path)
    val code = source.getLines().mkString("\n")
    source.close

    val result = Run.source(mainModule, code, env, Locals.empty)
    result.map(_ => env)
  }

  def source(mainModule: String, source: String, env: Env, oldLocals: Locals): Result[(Value, Locals)] =
    for {
      tokens <- Lexer.scanTokens(source)
      decls  <- Parser.fromTokens(tokens).parse.result
      locals <- Resolve.resolveAll(decls)(Context.default).map(_.locals ++ oldLocals)
      value  <- Interpreter.interpret(mainModule, decls, env, locals)
    } yield (value, locals)
}
