package com.melvic.dry.interpreter.values

import com.melvic.dry.interpreter.Env
import com.melvic.dry.result.Result.implicits.ToResult

trait Countable {
  def size: Int

  def addSizeMethod(env: Env): AddProperty =
    _ + ("size" -> Callable.noArg(env) {
      Value.Num(size).ok
    })
}
