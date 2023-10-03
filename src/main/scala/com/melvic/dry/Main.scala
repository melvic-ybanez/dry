package com.melvic.dry

import com.melvic.dry.interpreter.{Env, Run}
import com.melvic.dry.resolver.Scopes

object Main {
  def main(args: Array[String]): Unit =
    args match {
      case Array()     => Run.repl(Env.empty, Scopes.empty)
      case Array(path) => Run.mainModule(path)
      case _           => println("Usage: dry [script]")
    }
}
