package com.melvic.dry.interpreter

import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.interpreter.values.Callable
import com.melvic.dry.interpreter.values.Value.Num
import com.melvic.dry.result.Result.implicits.ToResult

import scala.Console._

object Tests {
  val TestCountName = "__tests_count"
  val SuccessCountName = "__tests_success_count"
  val FailureCountName = "__tests_failure_count"

  private[interpreter] def assert(env: Env): Callable =
    Callable(3, env) { case description :: value1 :: value2 :: _ =>
      getTestData(env).foreach { case (testsCount, successCount, failureCount) =>
        if (value1 == value2) {
          env.define(SuccessCountName, Num(successCount.value + 1))
          println(show"${Console.GREEN}[Success] $description${Console.RESET}")
        } else {
          env.define(FailureCountName, Num(failureCount.value + 1))
          System.err.println(show"[Failure] $description. Expected: $value1. Got: $value2")
        }

        env.define(TestCountName, Num(testsCount.value + 1))
      }
      Value.unit.ok
    }

  private[interpreter] def showTestResults(env: Env): Callable = Callable(0, env) { _ =>
    getTestData(env).foreach { case (testsCount, successCount, failureCount) =>
      System.out.println(
        show"Ran $testsCount tests. ${GREEN}Successful: $successCount. ${RED}Failed: $failureCount.${RESET}"
      )
    }
    Value.unit.ok
  }

  private def getTestData(env: Env): Option[(Num, Num, Num)] =
    for {
      testsCount   <- env.at(0, TestCountName).flatMap(_.toNum)
      successCount <- env.at(0, SuccessCountName).flatMap(_.toNum)
      failureCount <- env.at(0, FailureCountName).flatMap(_.toNum)
    } yield (testsCount, successCount, failureCount)
}
