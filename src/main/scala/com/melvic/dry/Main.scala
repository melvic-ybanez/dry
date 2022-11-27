package com.melvic.dry

import com.melvic.dry.interpreter.{Env, Run}

object Main {
  def main(args: Array[String]): Unit =
    args match {
      case Array()     => Run.repl(Env.empty)
      case Array(path) => Run.mainModule(path)
      case _           => println("Usage: dry [script]")
    }
}
