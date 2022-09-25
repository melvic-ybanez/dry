package com.melvic.dry.interpreter.eval

import com.melvic.dry.interpreter.values.Value

object implicits {
  implicit class EvalResultOps(result: EvalResult) {
    def map(f: Value => Value): EvalResult =
      result.andThen(_.map(f))

    def flatMap(f: Value => EvalResult): EvalResult = env =>
      result(env).flatMap { value =>
        f(value)(env)
      }
  }
}
