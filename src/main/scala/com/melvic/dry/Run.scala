package com.melvic.dry

import com.melvic.dry.Error.RuntimeError
import com.melvic.dry.Main.interpret

import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Run {
  @tailrec
  def prompt(): Unit = {
    val input = readLine("> ")
    if (input == "exit") ()
    else {
      Run.source(input).foreach(System.err.println)
      prompt()
    }
  }

  def path(path: String): Unit = {
    val bytes  = Files.readAllBytes(Paths.get(path))
    val result = Run.source(bytes.toString)
    result match {
      case Some(RuntimeError(_, _)) => System.exit(70)
      case Some(_)                  => System.exit(65)
    }
  }

  def source(source: String): Option[Error] = {
    val result = for {
      tokens <- Lexer.scanTokens(source)
      expr   <- Parser.fromTokens(tokens).parse
      value  <- interpret(expr)
    } yield println(value)
    result.left.toOption
  }
}
