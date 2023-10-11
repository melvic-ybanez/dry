package com.melvic.dry.interpreter

import com.melvic.dry.aux.Nel
import com.melvic.dry.resolver.Scopes
import com.melvic.dry.result.Failure

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
      def runExpression(scriptFailures: Nel[Failure]): Unit =
        Interpret.expression(input, env, scopes) match {
          case Left(exprFailures) =>
            writeFailures((exprFailures ++ scriptFailures).toList.distinct)
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
}

object Repl {
  private class LiveRepl extends Repl {
    override def writeSuccess(value: Value): Unit =
      println(s"${Console.GREEN}${Value.show(value)}${Console.RESET}")

    override def continue(env: Env, scopes: Scopes): Unit = {
      val input = readLine(s"${Console.YELLOW}dry> ${Console.RESET}")
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
      failures.foreach(failure => System.err.println(Failure.show(failure)))
  }

  def live: Repl = new LiveRepl
}
