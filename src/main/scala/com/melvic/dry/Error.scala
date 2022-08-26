package com.melvic.dry

sealed trait Error {
  def line: Int
}

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
      case InvalidCharacter(line, c) => showLineAndMessage(line, s"Invalid character: $c")
      case UnterminatedString(line) => showLineAndMessage(line, "Unterminated string")
    }

  def showFullLine(line: Int, where: String, message: String): String =
    s"[line $line] Error $where: $message"

  def showLineAndMessage(line: Int, message: String): String =
    showFullLine(line, "", message)
}
