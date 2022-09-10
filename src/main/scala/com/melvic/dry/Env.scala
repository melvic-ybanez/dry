package com.melvic.dry

import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.Result

final case class Env(table: Map[String, Value]) {

  /**
   * Adds a new variable to the environment. Note that this allows variable redefinitions.
   */
  def register(name: String, value: Value): Env =
    Env(table = table + (name -> value))

  def assign(name: Token, value: Value): Result[Env] =
    if (table.contains(name.lexeme)) Result.succeed(register(name.lexeme, value))
    else Result.fail(RuntimeError.undefinedVariable(name))

  def get(name: Token): Result[Value] =
    Result.fromOption(table.get(name.lexeme), RuntimeError.undefinedVariable(name))
}

object Env {
  def empty: Env = Env(Map())

  def get(name: Token): Env => Result[Value] =
    _.get(name)
}
