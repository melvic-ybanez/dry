package com.melvic.dry.eval

import com.melvic.dry.Value.{Bool, Str, None => VNone}
import com.melvic.dry.result.Result.Result
import com.melvic.dry.{Env, Value}
import com.melvic.dry.implicits._

object Evaluate extends EvalExpr with EvalDecl {
  type Evaluate[A]             = A => EvalResult
  type EvalResult              = EvalResultF[Value]
  type EvalResultF[+V] = Env => EvalOutF[V]
  type EvalOut                 = EvalOutF[Value]
  type EvalOutF[+V]    = Result[(V, Env)]

  def isTruthy(value: Value): Boolean =
    value match {
      case VNone       => false
      case Str("")     => false
      case Bool(value) => value
      case _           => true
    }

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
        result.map(_.mapValue(f))
    }
  }
}
