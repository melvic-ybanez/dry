package com.melvic.dry

import com.melvic.dry.Result.Result

object Main {
  def main(args: Array[String]): Unit =
    args match {
      case Array()     => Run.prompt()
      case Array(path) => Run.path(path)
      case _           => println("Usage: dry [script]")
    }

  def interpret(expr: Expr): Result[String] =
    Evaluate.expr(expr).map(Value.show)
}
