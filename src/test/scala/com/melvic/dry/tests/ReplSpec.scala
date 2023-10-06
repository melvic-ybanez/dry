package com.melvic.dry.tests

import com.melvic.dry.interpreter.{Env, Repl, Value}
import com.melvic.dry.lexer.Lexemes
import com.melvic.dry.resolver.Scopes
import com.melvic.dry.result.Failure
import com.melvic.dry.tests.ReplSpec.{Active, Exited, TestRepl}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ReplSpec extends AnyFlatSpec with should.Matchers with ScalaCheckPropertyChecks {
  "REPL" should "evaluate literal values to themselves" in forAll {
    (i: Int, d: Double, s: String, b: Boolean) =>
      assertValues(i, d, s, b)((testRepl, value) => testRepl.eval(value))
  }

  it should "quit when the 'exit' command is entered" in {
    val repl = TestRepl.default
    repl.eval("exit")
    repl.status should be(Exited)
  }

  it should "allow let-declared variables to persist" in forAll {
    (i: Int, d: Double, s: String, b: Boolean) =>
      assertValues(i, d, s, b) { (testRepl, input) =>
        testRepl.eval(s"let x = $input;")
        testRepl.eval("x")
      }
  }

  it should "allow declared classes to persist" in forAll { (age: Int) =>
    val repl = TestRepl.default
    repl.eval("class Person {}")
    repl.eval("let p = Person();")
    repl.eval(s"p.age = $age;")
    repl.eval("p.age")
    assertValue(repl, Value.Num(age))
  }

  it should "allow declared functions to persist" in forAll { x: Int =>
    val repl = TestRepl.default
    repl.eval("def half(x) { return x / 2; }")
    repl.eval(s"half($x)")
    assertValue(repl, Value.Num(x.toDouble / 2))
  }

  it should "evaluate expressions and print their results" in forAll { (_x: Int, _y: Int, _z: Int) =>
    // let's just limit the values to avoid funny formatting of large numbers for now
    val x = _x % 1000
    val y = _y % 1000
    val z = _z % 1000

    val repl = TestRepl.default

    repl.eval(s"$x + $y * $z / 5 ")
    repl.lastValue should be(Some(Value.Num(x + y * z.toDouble / 5)))

    repl.eval(s"$x % 10")
    repl.lastValue should be(Some(Value.Num(x % 10)))

    // TODO: Add tests for other operators (e.g. bitwise operators, logical operators, etc.)
  }

  def assertValues(i: Int, d: Double, s: String, b: Boolean)(run: (TestRepl, String) => Unit): Unit = {
    def testValue(input: String, value: Value): Unit = {
      val repl = TestRepl.default
      run(repl, input)
      assertValue(repl, value)
    }

    // Dry's double do not support numbers with exponents yet
    val validDouble = {
      val dStr = d.toString
      val validDStr =
        if (dStr.length < 2) dStr
        else {
          // the first character can be a sign (e.g. '-') so we let it pass
          dStr.head.toString + dStr.tail.takeWhile(c => c == '.' || c.isDigit)
        }
      validDStr.toDouble
    }

    testValue(i.toString, Value.Num(i))
    testValue(validDouble.toString, Value.Num(validDouble))
    testValue(s""""$s"""", Value.Str(s))
    testValue(if (b) Lexemes.True else Lexemes.False, Value.Bool(b))
  }

  def assertValue(repl: TestRepl, value: Value): Unit = {
    repl.status should be(Active)
    repl.lastValue should be(Some(value))
  }
}

object ReplSpec {
  sealed trait Status

  case object Active extends Status
  case object Exited extends Status

  class TestRepl(var lastValue: Option[Value], var env: Env, var scopes: Scopes, var status: Status)
      extends Repl {
    override def writeSuccess(value: Value): Unit =
      lastValue = Some(value)

    override def continue(env: Env, scopes: Scopes): Unit = {
      status = Active
      this.env = env
      this.scopes = scopes
    }

    override def exit(): Unit =
      status = Exited

    def eval(input: String): Unit =
      start(input, env, scopes)

    override def displayWelcomeMessage(): Unit = ()

    override def writeFailures(failures: List[Failure]): Unit =
      ()
  }

  object TestRepl {
    def default: TestRepl = new TestRepl(None, Env.empty, Scopes.empty, Active)
  }
}
