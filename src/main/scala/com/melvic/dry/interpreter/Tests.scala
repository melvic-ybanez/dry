package com.melvic.dry.interpreter

import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.interpreter.Env.Keys.{SuccessCount, TestCount}
import com.melvic.dry.interpreter.values.Callable
import com.melvic.dry.interpreter.values.Value.Num
import com.melvic.dry.result.Result.implicits.ToResult

import scala.Console._

object Tests {
  private[interpreter] def assert(env: Env): Callable =
    Callable(3, env) { case description :: value1 :: value2 :: _ =>
      getTestData(env).foreach { case (testsCount, successCount, _) =>
        if (value1 == value2) {
          env.define(SuccessCount, Num(successCount.value + 1))
          println(show"${Console.GREEN}[Success] $description${Console.RESET}")
        } else System.err.println(show"[Failure] $description. Expected: $value1. Got: $value2")

        env.define(TestCount, Num(testsCount.value + 1))
      }
      Value.unit.ok
    }

  private[interpreter] def showTestResults(env: Env): Callable = Callable.noArg(env) {
    getTestData(env).foreach { case (testsCount, successCount, failureCount) =>
      System.out.println(
        show"Ran $testsCount tests. ${GREEN}Successful: $successCount. ${RED}Failed: $failureCount.${RESET}"
      )
    }
    Value.unit.ok
  }

  private def getTestData(env: Env): Option[(Num, Num, Num)] =
    for {
      testsCount   <- env.at(0, TestCount).flatMap(_.toNum)
      successCount <- env.at(0, SuccessCount).flatMap(_.toNum)
    } yield (testsCount, successCount, Num(testsCount.value - successCount.value))
}
