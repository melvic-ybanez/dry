package com.melvic.dry.result

import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Expr
import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.aux.implicits.ListOps
import com.melvic.dry.result.Failure.RuntimeError.Kind
import com.melvic.dry.result.Failure.RuntimeError.Kind._
import com.melvic.dry.result.Result.Result
import com.melvic.dry.{Show, Token}

sealed trait Failure

object Failure {
  final case class Line(line: Int, where: String, message: String) extends Failure

  sealed trait LexerError extends Failure

  def line(line: Int, message: String): Failure =
    Line(line, "", message)

  private def showFullLine(line: Int, where: String, message: String): String =
    s"${showLine(line)} Error $where: $message"

  private def showLineAndMessage(line: Int, message: String): String =
    showFullLine(line, "", message)

  object LexerError {
    private final case class InvalidCharacter(line: Int, char: Char) extends LexerError
    private final case class UnterminatedString(line: Int) extends LexerError

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

  final case class RuntimeError(kind: Kind, token: Token, message: String) extends Failure

  object RuntimeError {
    sealed trait Kind {
      def apply(token: Token, message: String): RuntimeError =
        RuntimeError(this, token, message)

      val name: String = this.toString
    }

    object Kind {
      case object DivisionByZero extends Kind
      case object InvalidOperand extends Kind
      case object InvalidOperands extends Kind
      case object UndefinedVariable extends Kind
      case object NotCallable extends Kind
      case object IncorrectArity extends Kind
      case object DoesNotHaveProperties extends Kind
      case object UndefinedProperty extends Kind
      case object UndefinedKey extends Kind
      case object CanNotApplyIndexOperator extends Kind
      case object IndexOutOfBounds extends Kind
      case object InvalidIndex extends Kind
      case object InvalidArgument extends Kind
      case object ModuleNotFound extends Kind
    }

    def divisionByZero(token: Token, message: String): RuntimeError =
      DivisionByZero(token, message)

    def divisionByZero(token: Token): RuntimeError =
      divisionByZero(token, "Division by zero")

    def invalidOperand(operator: Token, message: String): RuntimeError =
      InvalidOperand(operator, message)

    def invalidOperand(operator: Token, expected: List[String]): RuntimeError =
      invalidOperand(operator, s"The operand must be any of the following: ${expected.toCsv}")

    def invalidOperands(operator: Token, message: String): RuntimeError =
      InvalidOperands(operator, message)

    def invalidOperands(operator: Token, expected: List[String]): RuntimeError =
      invalidOperands(operator, s"All operands must be any of the following: ${expected.toCsv}")

    def undefinedVariable(token: Token, message: String): RuntimeError =
      UndefinedVariable(token, message)

    def undefinedVariable(token: Token): RuntimeError =
      undefinedVariable(token, show"Undefined variable: $token")

    def notCallable(token: Token, message: String = "This expression is not callable."): RuntimeError =
      NotCallable(token, message)

    def incorrectArity(token: Token, message: String): RuntimeError =
      IncorrectArity(token, message)

    def incorrectArity(token: Token, expected: Int, got: Int): RuntimeError =
      incorrectArity(token, s"Incorrect arity. Expected: $expected. Got: $got")

    def doesNotHaveProperties(token: Token, message: String): RuntimeError =
      DoesNotHaveProperties(token, message)

    def doesNotHaveProperties(obj: Expr, token: Token): RuntimeError =
      doesNotHaveProperties(token, show"$obj does not have properties or fields.")

    def undefinedProperty(token: Token, message: String): RuntimeError =
      UndefinedProperty(token, message)

    def undefinedProperty(token: Token): RuntimeError =
      undefinedProperty(token, show"Undefined property: $token")

    def undefinedKey(token: Token, message: String): RuntimeError =
      UndefinedKey(token, message)

    def undefinedKey(key: Expr, token: Token): RuntimeError =
      undefinedKey(token, show"Undefined key: $key")

    def canNotApplyIndexOperator(token: Token, message: String): RuntimeError =
      CanNotApplyIndexOperator(token, message)

    def canNotApplyIndexOperator(obj: Expr, token: Token): RuntimeError =
      canNotApplyIndexOperator(token, show"Can not apply [] operator to $obj")

    def indexOutOfBounds(line: Int, message: String): RuntimeError =
      IndexOutOfBounds(Token.fromLine(line), message)

    def indexOutOfBounds(index: Int, line: Int): RuntimeError =
      indexOutOfBounds(line, show"Runtime Error. Index out of bounds: $index\n[line $line].")

    def invalidIndex(token: Token, message: String): RuntimeError =
      InvalidIndex(token, message)

    def invalidIndex(index: Expr, token: Token): RuntimeError =
      invalidIndex(token, show"Invalid index: $index")

    def invalidArgument(line: Int, message: String): RuntimeError =
      InvalidArgument(Token.fromLine(line), message)

    def invalidArgument(expected: String, got: String, line: Int): RuntimeError =
      invalidArgument(
        line,
        show"Runtime Error. Invalid argument. Expected: $expected. Got: $got\n${showLine(line)}."
      )

    def moduleNotFound(token: Token, message: String): RuntimeError =
      ModuleNotFound(token, message)

    def moduleNotFound(name: String, token: Token): RuntimeError =
      moduleNotFound(token, show"Module not found: $name")

    def show: Show[RuntimeError] = { case RuntimeError(_, token, message) =>
      show"Runtime Error: $message\n${showLine(token.line)}. $token"
    }
  }

  sealed trait ResolverError extends Failure

  object ResolverError {
    private final case class VariableAlreadyDefined(name: Token) extends ResolverError
    private final case class DeclaredButNotDefined(name: Token) extends ResolverError
    private final case class NotInsideAFunction(keyword: Token) extends ResolverError
    private final case class NotInsideAClass(keyword: Token) extends ResolverError
    private final case class ReturnFromInit(keyword: Token) extends ResolverError

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

  private def showLine(line: Int): String =
    s"[line $line]"
}
