package com.melvic.dry.interpreter

import com.melvic.dry.resolver.Scopes
import com.melvic.dry.result.{Failure, Result}

import scala.io.StdIn.readLine
import scala.util.chaining.scalaUtilChainingOps

trait Repl {
  def write(value: Value): Unit

  def continue(env: Env, scopes: Scopes): Unit

  def exit(): Unit

  def displayWelcomeMessage(): Unit

  def start(env: Env, scopes: Scopes): Unit = {
    displayWelcomeMessage()
    continue(env, scopes)
  }

  def start(input: String, env: Env, scopes: Scopes): Unit =
    if (input == "exit") exit()
    else {
      def runScript(): Unit =
        Interpret
          .script(input, env, scopes, Nil)
          .map { case (value, scopes) =>
            if (value != Value.Unit) write(value)
            scopes
          }
          .pipe { result =>
            Result.foreachFailure(result)(error => System.err.println(Failure.show(error)))
            result match {
              case Left(_)          => continue(env, scopes)
              case Right(newScopes) => continue(env, newScopes)
            }
          }

      Interpret.expression(input, env, scopes) match {
        case Left(_) => runScript()
        case Right(value) =>
          write(value)
          continue(env, scopes)
      }
    }
}

object Repl {
  private class LiveRepl extends Repl {
    override def write(value: Value): Unit =
      println(Value.show(value))

    override def continue(env: Env, scopes: Scopes): Unit = {
      val input = readLine("dry> ")
      start(input, env, scopes)
    }

    override def exit(): Unit = ()

    override def displayWelcomeMessage(): Unit =
      println("Welcome to Dry.\nType in expressions and statements for evaluation. Type 'exit' to quit.")
  }

  def live: Repl = new LiveRepl
}
