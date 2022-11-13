package com.melvic.dry.result

import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Expr
import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.aux.implicits.ListOps
import com.melvic.dry.result.Result.Result
import com.melvic.dry.{Show, Token}

sealed trait Failure

object Failure {
  final case class Line(line: Int, where: String, message: String) extends Failure

  sealed trait LexerError extends Failure

  def line(line: Int, message: String): Failure =
    Line(line, "", message)

  def showFullLine(line: Int, where: String, message: String): String =
    s"[line $line] Error $where: $message"

  def showLineAndMessage(line: Int, message: String): String =
    showFullLine(line, "", message)

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

  sealed trait ParseError extends Failure

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
        showLineAndMessage(assignment.line, "Parser Error: Invalid assignment target")
    }
  }

  sealed trait RuntimeError extends Failure {
    def token: Token
  }

  object RuntimeError {
    final case class DivisionByZero(token: Token) extends RuntimeError
    final case class InvalidOperand(token: Token, expected: List[String]) extends RuntimeError
    final case class InvalidOperands(token: Token, expected: List[String]) extends RuntimeError
    final case class UndefinedVariable(token: Token) extends RuntimeError
    final case class NotCallable(token: Token) extends RuntimeError
    final case class IncorrectArity(token: Token, expected: Int, got: Int) extends RuntimeError
    final case class DoesNotHaveProperties(obj: Expr, token: Token) extends RuntimeError
    final case class UndefinedProperty(token: Token) extends RuntimeError

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

    def doesNotHaveProperties(obj: Expr, token: Token): RuntimeError =
      DoesNotHaveProperties(obj, token)

    def undefinedProperty(token: Token): RuntimeError =
      UndefinedProperty(token)

    def show: Show[RuntimeError] = {
      case DivisionByZero(token) => errorMsg(token, "Division by zero")
      case InvalidOperand(token, expected) =>
        errorMsg(token, s"The operand must be any of the following: ${expected.toCsv}")
      case InvalidOperands(token, expected) =>
        errorMsg(token, s"All operands must be any of the following: ${expected.toCsv}")
      case UndefinedVariable(token) => errorMsg(token, show"Undefined variable: $token")
      case NotCallable(token)       => errorMsg(token, "This expression is not callable.")
      case IncorrectArity(token, expected, got) =>
        errorMsg(token, s"Incorrect arity. Expected: $expected. Got: $got")
      case DoesNotHaveProperties(obj, token) =>
        errorMsg(token, show"$obj does not have properties or fields.")
      case UndefinedProperty(token) => errorMsg(token, show"Undefined property: $token")
    }

    private def errorMsg(token: Token, message: String): String =
      show"Runtime Error: $message\n[line ${token.line}]. $token"
  }

  sealed trait ResolverError extends Failure

  object ResolverError {
    final case class VariableAlreadyDefined(name: Token) extends ResolverError
    final case class DeclaredButNotDefined(name: Token) extends ResolverError
    final case class NotInsideAFunction(keyword: Token) extends ResolverError
    final case class NotInsideAClass(keyword: Token) extends ResolverError
    final case class ReturnFromInit(keyword: Token) extends ResolverError

    def variableAlreadyDefined(name: Token): ResolverError =
      VariableAlreadyDefined(name)

    def declaredButNotDefined(name: Token): ResolverError =
      DeclaredButNotDefined(name)

    def notInsideAFunction(keyword: Token): ResolverError =
      NotInsideAFunction(keyword)

    def notInsideAClass(keyword: Token): ResolverError =
      NotInsideAClass(keyword)

    def returnFromInit(keyword: Token): ResolverError =
      ReturnFromInit(keyword)

    def show: Show[ResolverError] = {
      case VariableAlreadyDefined(name) =>
        errorMsg(name, show"Variable $name is already defined in this scope")
      case DeclaredButNotDefined(name) => errorMsg(name, show"'$name' is declared but not yet defined")
      case NotInsideAFunction(keyword) => errorMsg(keyword, show"'$keyword' is not inside a function")
      case NotInsideAClass(keyword)    => errorMsg(keyword, show"'$keyword' is not inside a class")
      case ReturnFromInit(keyword) => errorMsg(keyword, show"'return' is not allowed inside a constructor")
    }

    private def errorMsg(token: Token, message: String): String =
      showLineAndMessage(token.line, s"Resolver Error: $message")
  }

  def show: Show[Failure] = {
    case Line(line, where, message)     => showFullLine(line, where, message)
    case lexerError: LexerError         => LexerError.show(lexerError)
    case parseError: ParseError         => ParseError.show(parseError)
    case runtimeError: RuntimeError     => RuntimeError.show(runtimeError)
    case resolutionError: ResolverError => ResolverError.show(resolutionError)
  }

  implicit class FailureOps(failure: Failure) {
    def fail[A]: Result[A] = Result.fail(failure)
  }
}
