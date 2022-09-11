package com.melvic.dry.interpreter

import com.melvic.dry.parsers.Parser
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result.Result
import com.melvic.dry.result.{Failure, Result}
import com.melvic.dry.{Lexer, Value}

import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.chaining.scalaUtilChainingOps

object Run {
  @tailrec
  def repl(): Unit = {
    val input = readLine("> ")
    if (input == "exit") ()
    else {
      Run
        .source {
          // this is a trick I'm using to make repl accept expressions that
          // do not end with semicolons.
          if (input.endsWith(";")) input else input + ";"
        }
        .map {
          case Value.Unit => () // this is to avoid extra blank lines in the output
          case value => println(Value.show(value))
        }
        .pipe(Result.foreachFailure(_)(error => System.err.println(Failure.show(error))))
      repl()
    }
  }

  def path(path: String): Unit = {
    val source = Source.fromFile(path)
    val code   = source.getLines.mkString("\n")

    def reportAndExit(error: Failure, code: Int): Unit = {
      System.err.println(Failure.show(error))
      System.exit(code)
    }

    val result = Run.source(code)
    Result.foreachFailure(result) {
      case error: RuntimeError => reportAndExit(error, 70)
      case error               => reportAndExit(error, 65)
    }
    source.close
  }

  def source(source: String): Result[Value] =
    for {
      tokens <- Lexer.scanTokens(source)
      decls  <- Parser.fromTokens(tokens).parse.result
      value  <- Interpreter.interpret(decls)
    } yield value
}
