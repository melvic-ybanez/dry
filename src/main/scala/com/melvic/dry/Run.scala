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
    result foreach {
      case error: RuntimeError => reportAndExit(error, 70)
      case error               => reportAndExit(error, 65)
    }
    source.close
  }

  def source(source: String): List[Error] = {
    val result = for {
      tokens <- Lexer.scanTokens(source)
      decls  <- Parser.fromTokens(tokens).parse
      _      <- Interpreter.interpret(decls)
    } yield ()
    result.left.toOption.getOrElse(Nil)
  }
}
