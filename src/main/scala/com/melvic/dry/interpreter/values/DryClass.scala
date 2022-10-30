package com.melvic.dry.interpreter.values

import com.melvic.dry.interpreter.Env
import com.melvic.dry.result.Result.implicits.ToResult

final case class DryClass(name: String, enclosing: Env) extends Callable {
  override def arity = 0

  override def call = _ => DryInstance.fromClass(this).ok
}
