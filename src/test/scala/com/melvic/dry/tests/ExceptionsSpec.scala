package com.melvic.dry.tests

import com.melvic.dry.Token
import com.melvic.dry.aux.Nel.One
import com.melvic.dry.interpreter.Run
import com.melvic.dry.result.Failure.RuntimeError
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ExceptionsSpec extends AnyFlatSpec with should.Matchers with ScalaCheckPropertyChecks with Inside {
  "Raising a DivisionByZero exception" should "return a runtime" in {
    checkException("DivisionByZero")(RuntimeError.divisionByZero)
  }

  "Raising a UndefinedVariable exception" should "return a runtime error" in {
    checkException("UndefinedVariable")(RuntimeError.undefinedVariable)
  }

  "Raising a InvalidOperand exception" should "return a runtime error" in {
    checkException("InvalidOperand")(RuntimeError.invalidOperand)
  }

  "Raising a InvalidOperands exception" should "return a runtime error" in {
    checkException("InvalidOperands")(RuntimeError.invalidOperands)
  }

  "Raising a NotCallable exception" should "return a runtime error" in {
    checkException("NotCallable")(RuntimeError.notCallable)
  }

  "Raising a IncorrectArity exception" should "return a runtime error" in {
    checkException("IncorrectArity")(RuntimeError.incorrectArity)
  }

  "Raising a DoesNotHaveProperties exception" should "return a runtime error" in {
    checkException("DoesNotHaveProperties")(RuntimeError.doesNotHaveProperties)
  }

  "Raising a UndefinedProperty exception" should "return a runtime error" in {
    checkException("UndefinedProperty")(RuntimeError.undefinedProperty)
  }

  "Raising a UndefinedKey exception" should "return a runtime error" in {
    checkException("UndefinedKey")(RuntimeError.undefinedKey)
  }

  "Raising a CanNotApplyIndexOperator exception" should "return a runtime error" in {
    checkException("CanNotApplyIndexOperator")(RuntimeError.canNotApplyIndexOperator)
  }

  "Raising a IndexOutOfBounds exception" should "return a runtime error" in {
    checkException("IndexOutOfBounds")((token, msg) => RuntimeError.indexOutOfBounds(token.line, msg))
  }

  "Raising a InvalidIndex exception" should "return a runtime error" in {
    checkException("InvalidIndex")(RuntimeError.invalidIndex)
  }

  "Raising a InvalidArgument exception" should "return a runtime error" in {
    checkException("InvalidArgument")((token, msg) => RuntimeError.invalidArgument(token.line, msg))
  }

  "Raising a ModuleNotFound exception" should "return a runtime error" in {
    checkException("ModuleNotFound")(RuntimeError.moduleNotFound)
  }

  def checkException(exception: String)(error: (Token, String) => RuntimeError): Unit = {
    val errorMsg = "No money for you!"

    val code =
      s"""|def get_money() {
          |  raise($exception("$errorMsg"));
          |  return 5;
          |}
          |
          |get_money();
          |""".stripMargin

    Run.code(code, Nil) should be(Left(One(error(Token.fromLine(2), errorMsg))))
  }
}
