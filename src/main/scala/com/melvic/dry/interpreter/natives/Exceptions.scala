package com.melvic.dry.interpreter.natives

import com.melvic.dry.interpreter.Env
import com.melvic.dry.interpreter.Env.Register
import com.melvic.dry.interpreter.errors.{RaisedError, RuntimeError}
import com.melvic.dry.interpreter.values.Value.{Types, typeOf}
import com.melvic.dry.interpreter.values.{Callable, DException, DInstance}

object Exceptions {
  def register: Register = { env =>
    RuntimeError.Kind.all.foldLeft(env.defineWith("raise", raise)) { (env, kind) =>
      env.defineWith(kind.exceptionName, DException(kind, _))
    }
  }

  private def raise(env: Env): Callable = Callable.withLineNo(1, env) { line =>
    {
      case (exception: DInstance) :: _ => RaisedError(exception).fail
      case got :: _ => RuntimeError.invalidArgument(Types.Exception, typeOf(got), line).fail
    }
  }
}
