package com.melvic.dry

import com.melvic.dry.interpreter.Run

object Main {
  def main(args: Array[String]): Unit =
    args match {
      case Array()     => Run.repl()
      case Array(path) => Run.path(path)
      case _           => println("Usage: dry [script]")
    }
}
