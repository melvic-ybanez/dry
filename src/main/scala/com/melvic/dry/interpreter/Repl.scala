package com.melvic.dry.interpreter

import com.melvic.dry.aux.Nel
import com.melvic.dry.resolver.Scopes
import com.melvic.dry.result.Failure
import com.melvic.dry.result.Failure.{LexerError, Line, ParseError, ResolverError, RuntimeError}

import scala.io.StdIn.readLine
import scala.util.chaining.scalaUtilChainingOps

trait Repl {
  def writeSuccess(value: Value): Unit

  def writeFailures(failures: List[Failure]): Unit

  def continue(env: Env, scopes: Scopes): Unit

  def exit(): Unit

  def displayWelcomeMessage(): Unit

  def start(env: Env, scopes: Scopes): Unit = {
    displayWelcomeMessage()
    continue(env, scopes)
  }

  def start(input: String, env: Env, scopes: Scopes): Unit =
    if (input == "exit") {
      writeSuccess(Value.Str("Bye!"))
      exit()
    } else {
      def runExpression(scriptErrors: Nel[Failure]): Unit =
        Interpret.expression(input, env, scopes) match {
          case Left(exprErrors) =>
            writeFailures(prioritizeErrors(exprErrors ++ scriptErrors))
            continue(env, scopes)
          case Right(value) =>
            writeSuccess(value)
            continue(env, scopes)
        }

      def runScript(): Unit =
        Interpret
          .script(input, env, scopes, Nil)
          .map { case (value, scopes) =>
            if (value != Value.Unit) writeSuccess(value)
            scopes
          }
          .pipe {
            case Left(scriptFailures) => runExpression(scriptFailures)
            case Right(newScopes)     => continue(env, newScopes)
          }

      runScript()
    }

  /**
   * Prioritizes the errors that occurred at the later stages of the interpreter pipeline. This is only needed
   * because right now, the REPL interprets the input in multiple ways, and we want to report the erroneous
   * paths that have gone the furthest. After all, the more stages the input has passed, the closer it is to
   * achieving legitimacy.
   */
  private def prioritizeErrors(errors: Nel[Failure]): List[Failure] = {
    val allErrors = errors.toList.distinct
    val priorities = allErrors.map {
      case _: RuntimeError  => 1
      case _: ResolverError => 2
      case _: ParseError    => 3
      case _: LexerError    => 4
      case _: Line          => 5
    }
    val priorityTable = allErrors.zip(priorities).groupBy(_._2)
    val topPrio = priorityTable.toList.minBy(_._1)
    topPrio._2.map(_._1)
  }
}

object Repl {
  private class LiveRepl extends Repl {
    override def writeSuccess(value: Value): Unit =
      println(s"${Console.GREEN}${Value.show(value)}${Console.RESET}")

    override def continue(env: Env, scopes: Scopes): Unit = {
      val input = readLine(s"${Console.MAGENTA}dry> ${Console.RESET}")
      start(input, env, scopes)
    }

    override def exit(): Unit = ()

    override def displayWelcomeMessage(): Unit =
      println(
        s"${Console.BLUE}Welcome to Dry.\n" +
          s"Type in expressions and statements for evaluation. " +
          s"Type 'exit' to quit.${Console.RESET}"
      )

    override def writeFailures(failures: List[Failure]): Unit =
      failures.foreach(failure =>
        System.err.println(s"${Console.RED}${Failure.show(failure)}${Console.RESET}")
      )
  }

  def live: Repl = new LiveRepl
}
