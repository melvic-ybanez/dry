package com.melvic.dry

import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit =
    args match {
      case Array()     => runPrompt()
      case Array(path) => runPath(path)
      case _           => println("Usage: dry [script]")
    }

  @tailrec
  def runPrompt(): Unit = {
    val input = readLine("> ")
    if (input == "exit") ()
    else {
      runSource(input).foreach(System.err.println)
      runPrompt()
    }
  }

  def runPath(path: String): Unit = {
    val bytes = Files.readAllBytes(Paths.get(path))
    if (runSource(bytes.toString).nonEmpty) System.exit(65)
  }

  def runSource(source: String): Option[Error] =
    Lexer.scanTokens(source).map(_.foreach(println)).left.toOption
}
