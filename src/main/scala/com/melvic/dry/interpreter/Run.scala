package com.melvic.dry.interpreter

import com.melvic.dry.eval.EvalOut
import com.melvic.dry.parsers.Parser
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.{Failure, Result}
import com.melvic.dry.{Env, Lexer, Value}

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
          case (Value.Unit, env) => repl(env) // this is to avoid extra blank lines in the output
          case (value, env) =>
            println(Value.show(value))
            repl(env)
        }
        .pipe(Result.foreachFailure(_)(error => System.err.println(Failure.show(error))))
    }
  }

  def path(path: String): Unit = {
    val source = Source.fromFile(path)
    val code = source.getLines.mkString("\n")

    def reportAndExit(error: Failure, code: Int): Unit = {
      System.err.println(Failure.show(error))
      System.exit(code)
    }

    val result = Run.source(code, Env.empty)
    Result.foreachFailure(result) {
      case error: RuntimeError => reportAndExit(error, 70)
      case error               => reportAndExit(error, 65)
    }
    source.close
  }

  def source(source: String, env: Env): EvalOut =
    for {
      tokens <- Lexer.scanTokens(source)
      decls  <- Parser.fromTokens(tokens).parse.result
      value  <- Interpreter.interpret(decls, env)
    } yield value
}
