package com.melvic.dry.interpreter

import com.melvic.dry.Lexer
import com.melvic.dry.parsers.Parser
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.{Failure, Nel}

import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readLine

object Run {
  @tailrec
  def prompt(): Unit = {
    val input = readLine("> ")
    if (input == "exit") ()
    else {
      Run.source(input).foreach(_.foreach(error => System.err.println(Failure.show(error))))
      prompt()
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
    result.foreach(_ foreach {
      case error: RuntimeError => reportAndExit(error, 70)
      case error               => reportAndExit(error, 65)
    })
    source.close
  }

  def source(source: String): Option[Nel[Failure]] = {
    val result = for {
      tokens <- Lexer.scanTokens(source)
      decls  <- Parser.fromTokens(tokens).parse.result
      _      <- Interpreter.interpret(decls)
    } yield ()
    result.left.toOption
  }
}
