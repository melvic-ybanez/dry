package com.melvic.dry.interpreter

import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.interpreter.Env.{GlobalEnv, LocalEnv, Table}
import com.melvic.dry.interpreter.errors.RuntimeError
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.Result
import com.melvic.dry.{Show, Token}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

/**
 * Represents the environment that houses the mapping between variable names and their corresponding values.
 */
sealed trait Env {

  /**
   * Represents the environment that houses the mapping between variable names and their corresponding values.
   */
  def table: Table

  /**
   * Adds a new variable to the environment.
   */
  def define(name: String, value: Value): Env

  def define(token: Token, value: Value): Env =
    define(token.lexeme, value)

  /**
   * Like [[define]], but allows the caller to access this environment. This will be useful for chaining
   * applications of [[define]] where each value relies on the updated environment (i.e. a cyclic dependency
   * between this environment and the value being defined).
   */
  def defineWith(name: String, f: Env => Value): Env =
    define(name, f(this))

  def get(name: Token): Result[Value]

  def assign(name: Token, value: Value): Result[Env] =
    if (table.contains(name.lexeme)) Result.succeed(define(name, value))
    else
      this match {
        case GlobalEnv(_)               => Result.fail(RuntimeError.undefinedVariable(name))
        case LocalEnv(table, enclosing) => enclosing.assign(name, value).map(LocalEnv(table, _))
      }

  /**
   * Returns the variable given a distance from the current environment to the closest enclosing ancestor.
   */
  def at(distance: Int, name: String): Option[Value] =
    ancestorAt(distance).table.get(name)

  def assignAt(distance: Int, name: Token, value: Value): Env =
    ancestorAt(distance).pipe { env =>
      env.table += (name.lexeme -> value)
      env
    }

  @tailrec
  private def ancestorAt(distance: Int): Env =
    if (distance == 0) this
    else
      this match {
        case GlobalEnv(_)           => this
        case LocalEnv(_, enclosing) => enclosing.ancestorAt(distance - 1)
      }

  def height: Int = {
    @tailrec
    def recurse(height: Int, env: Env): Int =
      env match {
        case GlobalEnv(_)           => height
        case LocalEnv(_, enclosing) => recurse(height + 1, enclosing)
      }

    recurse(1, this)
  }
}

object Env {
  type Table = mutable.Map[String, Value]
  type Register = Env => Env

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
      copy(table = table += (name -> value))

    override def get(name: Token): Result[Value] =
      Result.fromOption(table.get(name.lexeme), RuntimeError.undefinedVariable(name))
  }

  def empty: Env = GlobalEnv(mutable.Map())

  def get(name: Token): Env => Result[Value] =
    _.get(name)

  def fromEnclosing(enclosing: Env): Env =
    LocalEnv(mutable.Map(), enclosing)

  def show: Show[Env] = {
    def mapValues: Table => Map[String, String] = _.view.mapValues(Value.show).toMap

    {
      case GlobalEnv(table) => s"Global { table: ${mapValues(table)} }"
      case LocalEnv(table, enclosing) =>
        show"Local { table: ${mapValues(table)}, enclosing: $enclosing}"
    }
  }
}
