package com.melvic.dry

import com.melvic.dry.Env.{GlobalEnv, LocalEnv}
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.Result

/**
 * Represents the environment that houses the mapping between variable names and their corresponding values.
 */
sealed trait Env {
  def table: Map[String, Value]

  /**
   * Adds a new variable to the environment.
   */
  def set(name: String, value: Value): Env

  def get(name: Token): Result[Value]

  def assign(name: Token, value: Value): Result[Env] =
    if (table.contains(name.lexeme)) Result.succeed(set(name.lexeme, value))
    else
      this match {
        case GlobalEnv(_)       => Result.fail(RuntimeError.undefinedVariable(name))
        case localEnv: LocalEnv => localEnv.assign(name, value)
      }
}

object Env {

  /**
   * An [[Env]] that has a pointer to its immediate enclosing environment, forming a Cactus Stack, to enable
   * lexical scoping.
   */
  final case class LocalEnv(table: Map[String, Value], enclosing: Env) extends Env {

    /**
     * Adds a new variable to the environment.
     */
    def set(name: String, value: Value): LocalEnv =
      LocalEnv(table = table + (name -> value), enclosing)

    def get(name: Token): Result[Value] =
      table
        .get(name.lexeme)
        .fold(enclosing.get(name))(Result.succeed)
  }

  /**
   * A global [[Env]] that doesn't have an enclosing scope.
   * @param table
   */
  final case class GlobalEnv(table: Map[String, Value]) extends Env {

    /**
     * Adds a new variable to the environment. Note that this allows variable redefinitions.
     */
    override def set(name: String, value: Value): Env =
      GlobalEnv(table = table + (name -> value))

    override def get(name: Token): Result[Value] =
      Result.fromOption(table.get(name.lexeme), RuntimeError.undefinedVariable(name))
  }

  def empty: Env = GlobalEnv(Map())

  def get(name: Token): Env => Result[Value] =
    _.get(name)
}
