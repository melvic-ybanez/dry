package com.melvic.dry.interpreter

import com.melvic.dry.Token
import com.melvic.dry.interpreter.Env.{GlobalEnv, LocalEnv, Table}
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.Result

import scala.collection.mutable

/**
 * Represents the environment that houses the mapping between variable names and their corresponding values.
 */
sealed trait Env {
  def table: Table

  /**
   * Adds a new variable to the environment.
   */
  def define(name: String, value: Value): Env

  /**
   * Like [[define]], but allows the caller to access this environment. This will be useful for chaining
   * applications of [[define]] where every application relies on the previously created environment.
   */
  def defineWith(name: String, f: Env => Value): Env =
    define(name, f(this))

  def get(name: Token): Result[Value]

  def assign(name: Token, value: Value): Result[Env] =
    if (table.contains(name.lexeme)) Result.succeed(define(name.lexeme, value))
    else
      this match {
        case GlobalEnv(_)               => Result.fail(RuntimeError.undefinedVariable(name))
        case LocalEnv(table, enclosing) => enclosing.assign(name, value).map(LocalEnv(table, _))
      }
}

object Env {
  type Table = mutable.Map[String, Value]

  /**
   * An [[Env]] that has a pointer to its immediate enclosing environment, forming a Cactus Stack, to enable
   * lexical scoping.
   */
  final case class LocalEnv(table: Table, enclosing: Env) extends Env {

    /**
     * Adds a new variable to the environment.
     */
    def define(name: String, value: Value): LocalEnv =
      LocalEnv(table += (name -> value), enclosing)

    def get(name: Token): Result[Value] =
      table
        .get(name.lexeme)
        .fold(enclosing.get(name))(Result.succeed)
  }

  /**
   * A global [[Env]] that doesn't have an enclosing scope.
   */
  final case class GlobalEnv(table: Table) extends Env {

    /**
     * Adds a new variable to the environment. Note that this allows variable redefinitions.
     */
    override def define(name: String, value: Value): Env =
      GlobalEnv(table += (name -> value))

    override def get(name: Token): Result[Value] =
      Result.fromOption(table.get(name.lexeme), RuntimeError.undefinedVariable(name))
  }

  def empty: Env = GlobalEnv(mutable.Map())

  def get(name: Token): Env => Result[Value] =
    _.get(name)

  def fromEnclosing(enclosing: Env): Env =
    LocalEnv(mutable.Map(), enclosing)
}
