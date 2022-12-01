package com.melvic.dry

import com.melvic.dry.interpreter.{Env, Run}
import com.melvic.dry.resolver.Locals

object Main {
  def main(args: Array[String]): Unit =
    args match {
      case Array()     => Run.repl(Env.empty, Locals.empty)
      case Array(path) => Run.mainModule(path)
      case _           => println("Usage: dry [script]")
    }
}
