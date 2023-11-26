package com.melvic.dry.interpreter.errors

import com.melvic.dry.ast.Expr
import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.aux.implicits.ListOps
import com.melvic.dry.interpreter.errors.RuntimeError.Kind
import com.melvic.dry.interpreter.errors.RuntimeError.Kind._
import com.melvic.dry.result.Failure
import com.melvic.dry.result.Failure.showLine
import com.melvic.dry.{Show, Token}

final case class RuntimeError(kind: Kind, token: Token, message: String) extends Failure

object RuntimeError {
  sealed trait Kind {
    def apply(token: Token, message: String): RuntimeError =
      RuntimeError(this, token, message)

    val name: String = this.toString
    val exceptionName: String = name + "Error"
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

    def all: List[Kind] = DivisionByZero :: InvalidOperand :: InvalidOperands :: UndefinedVariable ::
      NotCallable :: IncorrectArity :: DoesNotHaveProperties :: UndefinedProperty ::
      UndefinedKey :: CanNotApplyIndexOperator :: IndexOutOfBounds :: InvalidIndex ::
      InvalidArgument :: ModuleNotFound :: Nil
  }

  def divisionByZero(token: Token): RuntimeError =
    DivisionByZero(token, "Division by zero")

  def invalidOperand(operator: Token, expected: List[String]): RuntimeError =
    InvalidOperand(operator, s"The operand must be any of the following: ${expected.toCsv}")

  def invalidOperands(operator: Token, expected: List[String]): RuntimeError =
    InvalidOperands(operator, s"All operands must be any of the following: ${expected.toCsv}")

  def undefinedVariable(token: Token): RuntimeError =
    UndefinedVariable(token, show"Undefined variable: $token")

  def notCallable(token: Token, message: String = "This expression is not callable."): RuntimeError =
    NotCallable(token, message)

  def incorrectArity(token: Token, expected: Int, got: Int): RuntimeError =
    IncorrectArity(token, s"Incorrect arity. Expected: $expected. Got: $got")

  def doesNotHaveProperties(obj: Expr, token: Token): RuntimeError =
    DoesNotHaveProperties(token, show"$obj does not have properties or fields.")

  def undefinedProperty(token: Token): RuntimeError =
    UndefinedProperty(token, show"Undefined property: $token")

  def undefinedKey(key: Expr, token: Token): RuntimeError =
    UndefinedKey(token, show"Undefined key: $key")

  def canNotApplyIndexOperator(obj: Expr, token: Token): RuntimeError =
    CanNotApplyIndexOperator(token, show"Can not apply [] operator to $obj")

  def indexOutOfBounds(line: Int, message: String): RuntimeError =
    IndexOutOfBounds(Token.fromLine(line), message)

  def indexOutOfBounds(index: Int, line: Int): RuntimeError =
    indexOutOfBounds(line, show"Runtime Error. Index out of bounds: $index\n[line $line].")

  def invalidIndex(index: Expr, token: Token): RuntimeError =
    InvalidIndex(token, show"Invalid index: $index")

  def invalidArgument(line: Int, message: String): RuntimeError =
    InvalidArgument(Token.fromLine(line), message)

  def invalidArgument(expected: String, got: String, line: Int): RuntimeError =
    invalidArgument(
      line,
      show"Runtime Error. Invalid argument. Expected: $expected. Got: $got\n${showLine(line)}."
    )

  def moduleNotFound(name: String, token: Token): RuntimeError =
    ModuleNotFound(token, show"Module not found: $name")

  def show: Show[RuntimeError] = { case RuntimeError(_, token, message) =>
    show"Runtime Error: $message\n${showLine(token.line)}. $token"
  }
}
