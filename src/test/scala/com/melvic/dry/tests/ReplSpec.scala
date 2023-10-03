package com.melvic.dry.tests

import com.melvic.dry.interpreter.{Env, Repl, Value}
import com.melvic.dry.lexer.Lexemes
import com.melvic.dry.resolver.{Locals, Scopes}
import com.melvic.dry.tests.ReplSpec.{Active, Exited, TestRepl}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ReplSpec extends AnyFlatSpec with should.Matchers with ScalaCheckPropertyChecks {
  "Literal values" should "evaluate to themselves" in forAll { (i: Int, d: Double, s: String, b: Boolean) =>
    val testRepl: TestRepl = TestRepl.default

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

    def testValue(input: String, value: Value): Unit = {
      testRepl.runInput(input)
      testRepl.lastValue should be(Some(value))
      testRepl.status should be(Active)
    }

    testValue(i.toString, Value.Num(i))
    testValue(validDouble.toString, Value.Num(validDouble))
    testValue(s""""$s"""", Value.Str(s))
    testValue(if (b) Lexemes.True else Lexemes.False, Value.Bool(b))
  }

  "The 'exit' command" should "exit the REPL" in {
    val testRepl = TestRepl.default
    testRepl.runInput("exit")
    testRepl.status should be(Exited)
  }
}

object ReplSpec {
  sealed trait Status

  case object Active extends Status
  case object Exited extends Status

  class TestRepl(var lastValue: Option[Value], var status: Status) extends Repl {
    override def write(value: Value): Unit =
      lastValue = Some(value)

    override def continue(env: Env, scopes: Scopes): Unit =
      status = Active

    override def exit(): Unit =
      status = Exited

    def runInput(input: String): Unit =
      start(input, Env.empty, Scopes.empty)
  }

  object TestRepl {
    def default: TestRepl = new TestRepl(None, Active)
  }
}
