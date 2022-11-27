package com.melvic.dry.interpreter.values

import com.melvic.dry.Token
import com.melvic.dry.interpreter.Env
import com.melvic.dry.result.Result.Result

final case class DModule(env: Env) extends Value {
  def get(name: Token): Result[Value] =
    env.get(name)

  def set(name: Token, value: Value): Value.None = {
    env.assign(name, value)
    Value.None
  }
}
