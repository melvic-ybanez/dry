package com.melvic.dry.tests

import com.melvic.dry.aux.Nel.One
import com.melvic.dry.interpreter.Run
import com.melvic.dry.result.Failure.RuntimeError
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ExceptionsSpec extends AnyFlatSpec with should.Matchers with ScalaCheckPropertyChecks with Inside {
  "Raising a DivisionByZero exception" should "return a runtime a runtime error" in {
    checkException("DivisionByZero")(RuntimeError.divisionByZero)
  }

  def checkException(exception: String)(error: (String, Int) => RuntimeError): Unit = {
    val errorMsg = "No money for you!"

    val code =
      s"""|def get_money() {
          |  raise($exception("$errorMsg"));
          |  return 5;
          |}
          |
          |get_money();
          |""".stripMargin

    Run.code(code, Nil) should be(Left(One(error(errorMsg, 2))))
  }
}
