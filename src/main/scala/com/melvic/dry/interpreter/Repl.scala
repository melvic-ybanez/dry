package com.melvic.dry.interpreter

import com.melvic.dry.resolver.Locals
import com.melvic.dry.result.{Failure, Result}

import scala.io.StdIn.readLine
import scala.util.chaining.scalaUtilChainingOps

sealed trait Repl {
  def read: String

  def write(value: Value): Unit

  def loop(env: Env, locals: Locals): Unit = {
    val input = read
    if (input == "exit") ()
    else {
      def runScript(): Unit =
        Interpret
          .script(input, env, locals, Nil)
          .map { case (value, locals) =>
            if (value != Value.Unit) write(value)
            locals
          }
          .pipe { result =>
            Result.foreachFailure(result)(error => System.err.println(Failure.show(error)))
            result match {
              case Left(_) => loop(env, locals)
              case Right(newLocals) => loop(env, newLocals)
            }
          }

      Interpret.expression(input, env) match {
        case Left(_) => runScript()
        case Right(value) =>
          write(value)
          loop(env, locals)
      }
    }
  }
}

object Repl {
  private class LiveRepl extends Repl {
    override def read: String = readLine("> ")

    override def write(value: Value): Unit =
      println(Value.show(value))
  }

  def live: Repl = new LiveRepl
}
