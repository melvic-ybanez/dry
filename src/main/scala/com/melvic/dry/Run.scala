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

    val result = Run.source(code)
    result match {
      case Some(_: RuntimeError) => System.exit(70)
      case Some(_)               => System.exit(65)
      case _                     => ()
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
