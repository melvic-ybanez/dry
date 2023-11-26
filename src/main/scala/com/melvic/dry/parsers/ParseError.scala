package com.melvic.dry.parsers

import com.melvic.dry.{Show, Token}
import com.melvic.dry.Token.TokenType
import com.melvic.dry.result.Failure
import com.melvic.dry.result.Failure.{showFullLine, showLineAndMessage}

sealed trait ParseError extends Failure

object ParseError {
  final case class Expected(start: Token, expected: String, where: String, at: String) extends ParseError

  final case class InvalidAssignmentTarget(assignment: Token) extends ParseError

  def expected(start: Token, expected: String, at: String): ParseError =
    if (start.tokenType == TokenType.Eof) Expected(start, expected, "at end", at)
    else Expected(start, expected, s"at '${start.lexeme}'", at)

  def invalidAssignmentTarget(assignment: Token): ParseError =
    InvalidAssignmentTarget(assignment)

  def show: Show[ParseError] = {
    case Expected(start, expected, where, at) =>
      showFullLine(start.line, where, s"Expected '$expected' $at.")
    case InvalidAssignmentTarget(assignment) =>
      showLineAndMessage(assignment.line, "Parser Error: Invalid assignment target")
  }
}
