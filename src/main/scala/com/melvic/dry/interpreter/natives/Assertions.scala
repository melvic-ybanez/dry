package com.melvic.dry.interpreter.natives

import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.interpreter.Env.Register
import com.melvic.dry.interpreter.natives.Keys.{SuccessCount, TestCount}
import com.melvic.dry.interpreter.values.Callable
import com.melvic.dry.interpreter.values.Value.{Num, ToValue, Types, typeOf}
import com.melvic.dry.interpreter.{Env, Value}
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result.implicits.ToResult

import scala.Console._

//noinspection NameBooleanParameters
object Assertions {

  /**
   * The most general assert statement for code that do not throw runtime errors.
   *
   * Note: `assert_equals`, `assert_true`, and `assert_false` are all defined in terms of this function.
   */
  private[interpreter] def assertTrueWithMessage(env: Env): Callable =
    Callable.withLineNo(3, env)(line => {
      case (description @ Value.Str(_)) :: Value.Bool(condition) :: Value.Str(errorMessage) :: _ =>
        getTestData(env).foreach { case (testsCount, successCount, _) =>
          if (condition) addSuccess(env, description, successCount)
          else displayError(description, errorMessage)
          updateTestsCount(env, testsCount)
        }
        Value.unit.ok
      case d :: c :: m :: _ =>
        RuntimeError
          .invalidArgument(
            s"(${Types.String}, ${Types.Boolean}, ${Types.String})",
            s"(${typeOf(d)}, ${typeOf(c)}, ${typeOf(m)})",
            line
          )
          .fail
    })

  private[interpreter] def showTestResults(env: Env): Callable = Callable.noArg(env) {
    getTestData(env).foreach { case (testsCount, successCount, failureCount) =>
      System.out.println(
        show"Ran $testsCount tests. ${GREEN}Successful: $successCount. ${RED}Failed: $failureCount.${RESET}"
      )
    }
    Value.unit.ok
  }

  def register: Register =
    _.defineWith("assert_true_with_message", Assertions.assertTrueWithMessage)
      .defineWith("show_test_results", Assertions.showTestResults)

  private def addSuccess(env: Env, description: Value, successCount: Num): Unit = {
    env.define(SuccessCount, Num(successCount.value + 1))
    displaySuccess(description)
  }

  private def displaySuccess(description: Value): Unit =
    println(show"${Console.GREEN}[Success] $description${Console.RESET}")

  private def displayError(description: Value, message: String): Unit =
    System.err.println(show"${Console.RED}[Failure] $description. $message${Console.RESET}")

  private def updateTestsCount(env: Env, testsCount: Num) =
    env.define(TestCount, Num(testsCount.value + 1))

  private def getTestData(env: Env): Option[(Num, Num, Num)] =
    for {
      testsCount   <- env.at(0, TestCount).flatMap(_.toNum)
      successCount <- env.at(0, SuccessCount).flatMap(_.toNum)
    } yield (testsCount, successCount, Num(testsCount.value - successCount.value))
}
