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

/**
 * Houses the test cases for the parsing phase that are difficult to implement using Dry's tests (because the
 * parser would fail in the first place).
 *
 * Note: many of the test cases here are based on the lox test suite:
 * https://github.com/munificent/craftinginterpreters/blob/master/test/
 */
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
    assertInvalidAssignmentTarget(error)
  }

  it should "not be an arithmetic expression" in {
    val error = interpret("""let a = "a"; let b = "b"; a + b = "value";""")
    assertInvalidAssignmentTarget(error)
  }

  it should "not be an expression with prefix operator" in {
    val error = interpret("""let a = "a"; !a = "value";""")
    assertInvalidAssignmentTarget(error)
  }

  it should "not work with 'self' as the lhs" in {
    val error = interpret("""class Foo { def foo() { self = "value"; }}""")
    assertInvalidAssignmentTarget(error)
  }

  def assertSuccess(result: Result[Value]): Unit =
    result should be(Right(Value.Unit))

  def assertInvalidAssignmentTarget(error: Result[Value]): Unit = {
    error should matchPattern { case Left(One(InvalidAssignmentTarget(_))) => }
  }
}

object ParserSpec {
  def interpret(source: String): Result[Value] =
    Interpret.script(source, Env.empty, Scopes.empty, Nil).map(_._1)
}
