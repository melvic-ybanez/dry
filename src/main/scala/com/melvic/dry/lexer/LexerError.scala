package com.melvic.dry.lexer

import com.melvic.dry.Show
import com.melvic.dry.result.Failure
import com.melvic.dry.result.Failure.showLineAndMessage

sealed trait LexerError extends Failure

object LexerError {
  private final case class InvalidCharacter(line: Int, char: Char) extends LexerError

  private final case class UnterminatedString(line: Int) extends LexerError

  def invalidCharacter(line: Int, char: Char): Failure =
    InvalidCharacter(line, char)

  def unterminatedString(line: Int): Failure =
    UnterminatedString(line)

  def show: Show[LexerError] = {
    case InvalidCharacter(line, c) => showLineAndMessage(line, s"Invalid character: $c")
    case UnterminatedString(line) => showLineAndMessage(line, "Unterminated string")
  }
}
