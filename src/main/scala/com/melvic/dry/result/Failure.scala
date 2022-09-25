package com.melvic.dry.result

import com.melvic.dry.Token.TokenType
import com.melvic.dry.implicits.ListOps
import com.melvic.dry.{Show, Token}

sealed trait Failure

object Failure {
  final case class Line(line: Int, where: String, message: String) extends Failure

  sealed trait LexerError extends Failure

  sealed trait ParseError extends Failure

  sealed trait RuntimeError extends Failure {
    def token: Token
  }

  final case class ResolutionError(line: Int, message: String) extends Failure

  def line(line: Int, message: String): Failure =
    Line(line, "", message)

  def showFullLine(line: Int, where: String, message: String): String =
    s"[line $line] Error $where: $message"

  def showLineAndMessage(line: Int, message: String): String =
    showFullLine(line, "", message)

  def showRuntimeError(token: Token, message: String): String =
    s"$message\n[line ${token.line}]. ${token.lexeme}"

  object LexerError {
    final case class InvalidCharacter(line: Int, char: Char) extends LexerError
    final case class UnterminatedString(line: Int) extends LexerError

    def invalidCharacter(line: Int, char: Char): Failure =
      InvalidCharacter(line, char)

    def unterminatedString(line: Int): Failure =
      UnterminatedString(line)

    def show: Show[LexerError] = {
      case InvalidCharacter(line, c) => showLineAndMessage(line, s"Invalid character: $c")
      case UnterminatedString(line)  => showLineAndMessage(line, "Unterminated string")
    }
  }

  object ParseError {
    final case class Expected(start: Token, expected: String, where: String, after: String) extends ParseError
    final case class InvalidAssignmentTarget(assignment: Token) extends ParseError

    def expected(start: Token, end: String, after: String): ParseError =
      if (start.tokenType == TokenType.Eof) Expected(start, end, "at end", after)
      else Expected(start, end, s"at '${start.lexeme}'", after)

    def invalidAssignmentTarget(assignment: Token): ParseError =
      InvalidAssignmentTarget(assignment)

    def show: Show[ParseError] = {
      case Expected(start, expected, where, after) =>
        showFullLine(start.line, where, s"Expected '$expected' after $after.")
      case InvalidAssignmentTarget(assignment) =>
        showLineAndMessage(assignment.line, "Invalid assignment target")
    }
  }

  object RuntimeError {
    final case class DivisionByZero(token: Token) extends RuntimeError
    final case class InvalidOperand(token: Token, expected: List[String]) extends RuntimeError
    final case class InvalidOperands(token: Token, expected: List[String]) extends RuntimeError
    final case class UndefinedVariable(token: Token) extends RuntimeError
    final case class NotCallable(token: Token) extends RuntimeError
    final case class IncorrectArity(token: Token, expected: Int, got: Int) extends RuntimeError

    def divisionByZero(token: Token): RuntimeError =
      DivisionByZero(token)

    def invalidOperand(operator: Token, expected: List[String]): RuntimeError =
      InvalidOperand(operator, expected)

    def invalidOperands(operator: Token, expected: List[String]): RuntimeError =
      InvalidOperands(operator, expected)

    def undefinedVariable(token: Token): RuntimeError =
      UndefinedVariable(token)

    def notCallable(token: Token): RuntimeError =
      NotCallable(token)

    def incorrectArity(token: Token, expected: Int, got: Int): RuntimeError =
      IncorrectArity(token, expected, got)

    def show: Show[RuntimeError] = {
      case DivisionByZero(token) => showRuntimeError(token, "Division by zero")
      case InvalidOperand(token, expected) =>
        showRuntimeError(token, s"The operand must be any of the following: ${expected.toCsv}")
      case InvalidOperands(token, expected) =>
        showRuntimeError(token, s"All operands must be any of the following: ${expected.toCsv}")
      case UndefinedVariable(token) => showRuntimeError(token, s"Undefined variable: ${token.lexeme}")
      case NotCallable(token)       => showRuntimeError(token, "This expression is not callable.")
      case IncorrectArity(token, expected, got) =>
        showRuntimeError(token, s"Incorrect arity. Expected: $expected. Got: $got")
    }
  }

  def show: Show[Failure] = {
    case Line(line, where, message)     => showFullLine(line, where, message)
    case lexerError: LexerError         => LexerError.show(lexerError)
    case parseError: ParseError         => ParseError.show(parseError)
    case runtimeError: RuntimeError     => RuntimeError.show(runtimeError)
    case ResolutionError(line, message) => showLineAndMessage(line, s"Resolution Error: $message")
  }
}
