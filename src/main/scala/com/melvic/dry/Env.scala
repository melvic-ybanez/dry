package com.melvic.dry

import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result.Result
import com.melvic.dry.result.{Failure, Result}

final case class Env(table: Map[String, Value]) {

  /**
   * Adds a new variable to the environment. Note that this allows variable redefinitions.
   */
  def register(name: String, value: Value): Env =
    Env(table = table + (name -> value))

  def get(name: Token): Result[Value] =
    Result.fromOption(table.get(name.lexeme), RuntimeError.undefinedVariable(name))
}

object Env {
  def empty: Env = Env(Map())
}
