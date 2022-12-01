package com.melvic.dry.interpreter

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.aux.Nel.{Many, One}
import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.interpreter.Keys.{SuccessCount, TestCount}
import com.melvic.dry.interpreter.values.Callable
import com.melvic.dry.interpreter.values.Value.{Num, Str, ToValue}
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Failure.RuntimeError._
import com.melvic.dry.result.Result.implicits.ToResult

import scala.Console._

object Assertions {
  private[interpreter] def assert(env: Env): Callable =
    Callable(3, env) { case description :: value1 :: value2 :: _ =>
      getTestData(env).foreach { case (testsCount, successCount, _) =>
        if (value1 == value2) {
          env.define(SuccessCount, Num(successCount.value + 1))
          displaySuccess(description)
        } else displayEqualError(description, value1, value2)

        env.define(TestCount, Num(testsCount.value + 1))
      }
      Value.unit.ok
    }

  private[interpreter] def assertError(env: Env): Callable =
    Callable.withLineNo(3, env) { lineNo =>
      lazy val token = Token(TokenType.LeftParen, "(", lineNo)

      {
        case description :: error :: Callable(0, _, call) :: _ =>
          getTestData(env).foreach { case (testsCount, successCount, _) =>
            def check(errorResult: Str): Unit =
              if (error == errorResult) {
                env.define(SuccessCount, Num(successCount.value + 1))
                displaySuccess(description)
              } else displayEqualError(description, error, errorResult)

            call(token)(Nil).fold(
              {
                case One(value: RuntimeError)    => check(Str(errorKey(value)))
                case Many(head: RuntimeError, _) => check(Str(errorKey(head)))
                case _                           => ()
              },
              _ => System.err.println(show"[Failure] $description. Expected error: $error.")
            )
            env.define(TestCount, Num(testsCount.value + 1))
          }
          Value.unit.ok
        case _ :: _ :: Callable(n, _, _) :: _ => RuntimeError.incorrectArity(token, 0, n).fail
        case _                                => RuntimeError.notCallable(token).fail
      }
    }

  private[interpreter] def showTestResults(env: Env): Callable = Callable.noArg(env) {
    getTestData(env).foreach { case (testsCount, successCount, failureCount) =>
      System.out.println(
        show"Ran $testsCount tests. ${GREEN}Successful: $successCount. ${RED}Failed: $failureCount.${RESET}"
      )
    }
    Value.unit.ok
  }

  private def displaySuccess(description: Value): Unit =
    println(show"${Console.GREEN}[Success] $description${Console.RESET}")

  private def displayEqualError(description: Value, value1: Value, value2: Value): Unit =
    System.err.println(show"[Failure] $description. Expected: $value1. Got: $value2")

  private def errorKey(failure: RuntimeError): String =
    failure match {
      case DivisionByZero(_)           => Keys.Errors.DivisionByZero
      case InvalidOperand(_, _)        => Keys.Errors.InvalidOperand
      case InvalidOperands(_, _)       => Keys.Errors.InvalidOperands
      case UndefinedVariable(_)        => Keys.Errors.UndefinedVariable
      case NotCallable(_)              => Keys.Errors.NotCallable
      case IncorrectArity(_, _, _)     => Keys.Errors.IncorrectArity
      case DoesNotHaveProperties(_, _) => Keys.Errors.DoesNotHaveProperties
      case IndexOutOfBounds(_, _)      => Keys.Errors.IndexOutOfBounds
      case InvalidIndex(_, _)          => Keys.Errors.InvalidIndex
      case InvalidArgument(_, _, _)    => Keys.Errors.InvalidArgument
      case UndefinedProperty(_)        => Keys.Errors.UndefinedProperty
      case ModuleNotFound(_, _)        => Keys.Errors.ModuleNotFound
    }

  private def getTestData(env: Env): Option[(Num, Num, Num)] =
    for {
      testsCount   <- env.at(0, TestCount).flatMap(_.toNum)
      successCount <- env.at(0, SuccessCount).flatMap(_.toNum)
    } yield (testsCount, successCount, Num(testsCount.value - successCount.value))
}
