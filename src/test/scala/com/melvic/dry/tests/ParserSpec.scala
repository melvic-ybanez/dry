package com.melvic.dry.tests

import com.melvic.dry.aux.Nel.One
import com.melvic.dry.interpreter.{Env, Interpret, Value}
import com.melvic.dry.resolver.Scopes
import com.melvic.dry.result.Failure.ParseError.{Expected, InvalidAssignmentTarget}
import com.melvic.dry.result.Result.Result
import com.melvic.dry.tests.ParserSpec.interpret
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ParserSpec extends AnyFlatSpec with should.Matchers with ScalaCheckPropertyChecks {
  "Return statement" should "end in a semicolon" in {
    val noSemicolon = "let sum = lambda(x, y) { return x + y };"
    val error = interpret(noSemicolon)
    error should matchPattern { case Left(One(Expected(_, ";", _, _))) => }

    val withSemicolon = "let sum = lambda(x, y) { return x + y; };"
    assertSuccess(interpret(withSemicolon))
  }

  "Assignment target" should "not be a paren-enclosed var" in {
    val error = interpret("""let a = "a"; (a) = "value";""")
    error should matchPattern { case Left(One(InvalidAssignmentTarget(_))) => }
  }

  it should "not be an arithmetic expression" in {
    val error = interpret("""let a = "a"; let b = "b"; a + b = "value";""")
    error should matchPattern { case Left(One(InvalidAssignmentTarget(_))) => }
  }

  it should "not be a expression with prefix operator" in {
    val error = interpret("""let a = "a"; !a = "value";""")
    error should matchPattern { case Left(One(InvalidAssignmentTarget(_))) => }
  }

  def assertSuccess(result: Result[Value]): Unit =
    result should be(Right(Value.Unit))
}

object ParserSpec {
  def interpret(source: String): Result[Value] =
    Interpret.script(source, Env.empty, Scopes.empty, Nil).map(_._1)
}
