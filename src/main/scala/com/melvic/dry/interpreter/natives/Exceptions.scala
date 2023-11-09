package com.melvic.dry.interpreter.natives

import com.melvic.dry.interpreter.Env
import com.melvic.dry.interpreter.Env.Register
import com.melvic.dry.interpreter.values.DException._
import com.melvic.dry.interpreter.values.Value.{Types, typeOf}
import com.melvic.dry.interpreter.values.{Callable, DException, DInstance, Value}
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result.Result

object Exceptions {
  def register: Register =
    _.defineWith("raise", raise)
      .defineWith(DivisionByZero.name, DException(DivisionByZero, _))

  private def raise(env: Env): Callable = Callable.withLineNo(1, env) { line =>
    def invalidArgument(got: Value): Result[Value] =
      RuntimeError.invalidArgument(Types.Exception, typeOf(got), line).fail

    {
      case (exception: DInstance) :: _ =>
        def message: String =
          DException.messageOf(exception).getOrElse("An exception occurred")

        DException.Kind.of(exception).fold(invalidArgument(exception)) { case DivisionByZero.name =>
          RuntimeError.divisionByZero(message, line).fail
        }
      case arg :: _ => invalidArgument(arg)
    }
  }
}
