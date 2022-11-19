package com.melvic.dry.interpreter

import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.interpreter.Env.{GlobalEnv, LocalEnv, Table}
import com.melvic.dry.resolver.Locals
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.Result
import com.melvic.dry.{Show, Token}

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
   * For backwards-compatibility, I'm putting the locals here. This would make it easier to access them via
   * any environment.
   */
  def locals: Locals

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
        case GlobalEnv(_, _)            => Result.fail(RuntimeError.undefinedVariable(name))
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

  def ancestorAt(distance: Int): Env =
    if (distance == 0) this
    else
      this match {
        case GlobalEnv(_, _)        => this
        case LocalEnv(_, enclosing) => enclosing.ancestorAt(distance - 1)
      }

  def withLocals(locals: Locals): Env =
    this match {
      case GlobalEnv(table, _)        => GlobalEnv(table, locals)
      case LocalEnv(table, enclosing) => LocalEnv(table, enclosing.withLocals(locals))
    }

  /**
   * This is typically used for debugging.
   */
  def height: Int =
    this match {
      case GlobalEnv(_, _)        => 1
      case LocalEnv(_, enclosing) => 1 + enclosing.height
    }
}

object Env {
  type Table = mutable.Map[String, Value]

  /**
   * An [[Env]] that has a pointer to its immediate enclosing environment, forming a Cactus Stack, to enable
   * lexical scoping.
   */
  final case class LocalEnv(table: Table, enclosing: Env) extends Env {
    def locals: Locals = enclosing.locals

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
  final case class GlobalEnv(table: Table, locals: Locals) extends Env {

    /**
     * Adds a new variable to the environment. Note that this allows variable redefinitions.
     */
    override def define(name: String, value: Value): Env =
      copy(table = table += (name -> value))

    override def get(name: Token): Result[Value] =
      Result.fromOption(table.get(name.lexeme), RuntimeError.undefinedVariable(name))
  }

  object Keys {
    val TestCount = "__tests_count__"
    val SuccessCount = "__tests_success_count__"
    val LineNumber = "__line_number__"
  }

  def empty: Env = GlobalEnv(mutable.Map(), Map())

  def get(name: Token): Env => Result[Value] =
    _.get(name)

  def fromEnclosing(enclosing: Env): Env =
    LocalEnv(mutable.Map(), enclosing)

  def show: Show[Env] = {
    def mapValues: Table => Map[String, String] = _.view.mapValues(Value.show).toMap

    {
      case GlobalEnv(table, _) => s"Global { table: ${mapValues(table)} }"
      case LocalEnv(table, enclosing) =>
        show"Local { table: ${mapValues(table)}, enclosing: $enclosing}"
    }
  }
}
