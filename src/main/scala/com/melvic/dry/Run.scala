package com.melvic.dry

import com.melvic.dry.Error.RuntimeError
import com.melvic.dry.parsers.Parser

import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readLine

object Run {
  @tailrec
  def prompt(): Unit = {
    val input = readLine("> ")
    if (input == "exit") ()
    else {
      Run.source(input).foreach(error => System.err.println(Error.show(error)))
      prompt()
    }
  }

  def path(path: String): Unit = {
    val source = Source.fromFile(path)
    val code   = source.getLines.mkString("\n")

    def reportAndExit(error: Error, code: Int): Unit = {
      System.err.println(Error.show(error))
      System.exit(code)
    }

    val result = Run.source(code)
    result match {
      case Some(error: RuntimeError) => reportAndExit(error, 70)
      case Some(error)               => reportAndExit(error, 65)
      case _                         => ()
    }
    source.close
  }

  def source(source: String): Option[Error] = {
    val result = for {
      tokens <- Lexer.scanTokens(source)
      stmts  <- Parser.fromTokens(tokens).parse
      _      <- Interpreter.interpret(stmts)
    } yield ()
    result.left.toOption
  }
}
