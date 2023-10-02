package com.melvic.dry.interpreter

import com.melvic.dry.resolver.Locals
import com.melvic.dry.result.{Failure, Result}

import scala.io.StdIn.readLine
import scala.util.chaining.scalaUtilChainingOps

trait Repl {
  def write(value: Value): Unit

  def continue(env: Env, locals: Locals): Unit

  def exit(): Unit

  def start(env: Env, locals: Locals): Unit =
    continue(env, locals)

  def start(input: String, env: Env, locals: Locals): Unit =
    if (input == "exit") exit()
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
              case Left(_) => continue(env, locals)
              case Right(newLocals) => continue(env, newLocals)
            }
          }

      Interpret.expression(input, env) match {
        case Left(_) => runScript()
        case Right(value) =>
          write(value)
          continue(env, locals)
      }
    }
}

object Repl {
  private class LiveRepl extends Repl {
    override def write(value: Value): Unit =
      println(Value.show(value))

    override def continue(env: Env, locals: Locals): Unit = {
      val input = readLine("repl> ")
      start(input, env, locals)
    }

    override def exit(): Unit = ()
  }

  def live: Repl = new LiveRepl
}
