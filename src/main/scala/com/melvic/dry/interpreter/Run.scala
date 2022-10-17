package com.melvic.dry.interpreter

import com.melvic.dry.Lexer
import com.melvic.dry.interpreter.eval.EvalOut
import com.melvic.dry.parsers.Parser
import com.melvic.dry.resolver.{FunctionType, Resolve}
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
          // this is a trick I'm using to make repl accept expressions that
          // do not end with semicolons.
          if (input.endsWith(";")) input else input + ";",
          env
        )
        .map {
          case Value.Unit => repl(env) // this is to avoid extra blank lines in the output
          case value =>
            println(Value.show(value))
            repl(env)
        }
        .pipe(Result.foreachFailure(_)(error => System.err.println(Failure.show(error))))
    }
  }

  def path(path: String): Unit = {
    val source = Source.fromFile(path)
    val code = source.getLines.mkString("\n")

    val result = Run.source(code, Env.empty)
    Result.foreachFailure(result)(error => System.err.println(Failure.show(error)))
    source.close
    System.exit(if (result.isLeft) -1 else 0)
  }

  def source(source: String, env: Env): EvalOut =
    for {
      tokens <- Lexer.scanTokens(source)
      decls  <- Parser.fromTokens(tokens).parse.result
      locals <- Resolve.resolveAll(decls)(Nil, Map(), FunctionType.None).map(_._2)
      value  <- Interpreter.interpret(decls, env, locals)
    } yield value
}
