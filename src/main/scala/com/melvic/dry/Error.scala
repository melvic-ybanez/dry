package com.melvic.dry

import com.melvic.dry.Error.ParseError.Expected
import com.melvic.dry.Token.TokenType

sealed trait Error

object Error {
  final case class Line(line: Int, where: String, message: String) extends Error
  final case class InvalidCharacter(line: Int, char: Char) extends Error
  final case class UnterminatedString(line: Int) extends Error

  def line(line: Int, message: String): Error =
    Line(line, "", message)

  def invalidCharacter(line: Int, char: Char): Error =
    InvalidCharacter(line, char)

  def unterminatedString(line: Int): Error =
    UnterminatedString(line)

  def show(error: Error): String =
    error match {
      case Line(line, where, message) => showFullLine(line, where, message)
      case InvalidCharacter(line, c)  => showLineAndMessage(line, s"Invalid character: $c")
      case UnterminatedString(line)   => showLineAndMessage(line, "Unterminated string")
      case Expected(start, expected, where)     => showFullLine(start.line, where, s"Expected $expected.")
    }

  def showFullLine(line: Int, where: String, message: String): String =
    s"[line $line] Error $where: $message"

  def showLineAndMessage(line: Int, message: String): String =
    showFullLine(line, "", message)

  sealed trait ParseError extends Error

  object ParseError {
    final case class Expected(start: Token, expected: String, where: String) extends ParseError

    def expected(start: Token, end: String): ParseError =
      if (start.tokenType == TokenType.Eof) Expected(start, end, "at end")
      else Expected(start, end, s" at '${start.lexeme}'")
  }
}
