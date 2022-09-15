package com.melvic.dry.eval

import com.melvic.dry.result.Result.Result
import com.melvic.dry.{Env, Value}

object implicits {
  implicit class EvalOutOps(result: EvalOut) {
    def mapValue(f: Value => Value): EvalOut =
      result.map { case (a, env) =>
        (f(a), env)
      }

    def flatMapValue(f: Value => Result[Value]): EvalOut =
      result.flatMap { case (a, env) =>
        f(a).map(value => (value, env))
      }
  }

  implicit class EvalOutContentOps(content: (Value, Env)) {
    def env: Env     = content._2
    def value: Value = content._1
  }

  implicit class ResultOps[V](result: Result[V]) {
    def withEnv: EvalResultF[V] =
      env => result.map((_, env))
  }

  implicit class EvalResultOps(result: EvalResult) {
    def mapValue(f: Value => Value): EvalResult =
      result.andThen(_.mapValue(f))

    def flatMapValue(f: Value => EvalResult): EvalResult =
      result.andThen(_.flatMap { case (value, env) =>
        f(value)(env)
      })
  }
}
