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
  def repl(env: Env): Unit = {
    val input = readLine("> ")
    if (input == "exit") ()
    else {
      Run
        .source(
          ".",
          // this is a trick I'm using to make repl accept expressions that
          // do not end with semicolons.
          if (!input.endsWith(";")  && !input.endsWith("}")) input + ";"
          else input,
          env
        )
        .map { case (value, _) =>
          if (value != Value.Unit)
            println(Value.show(value))
        }
        .pipe(Result.foreachFailure(_)(error => System.err.println(Failure.show(error))))
      repl(env)
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

    val result = Run.source(mainModule, code, env)
    result.map { case (_, locals) => env.withLocals(locals) }
  }

  def source(mainModule: String, source: String, env: Env): Result[(Value, Locals)] =
    for {
      tokens <- Lexer.scanTokens(source)
      decls  <- Parser.fromTokens(tokens).parse.result
      locals <- Resolve.resolveAll(decls)(Context.default).map(_.locals)
      value  <- Interpreter.interpret(mainModule, decls, env, locals)
    } yield (value, locals)
}
