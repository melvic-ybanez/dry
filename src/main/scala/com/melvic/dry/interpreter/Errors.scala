package com.melvic.dry.interpreter

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.interpreter.values.Callable
import com.melvic.dry.interpreter.values.Value.RuntimeError
import com.melvic.dry.result.Failure.{RuntimeError => FRuntimeError}
import com.melvic.dry.result.Result.implicits.ToResult

import scala.collection.mutable

object Errors {
  val DivisionByZeroName = "division_by_zero"

  def all(env: Env): mutable.Map[String, Value] = mutable.Map(
    DivisionByZeroName -> divisionByZero(env)
  )

  def divisionByZero(env: Env): Value =
    Callable.withLineNo(0, env)(lineNo =>
      _ => error(FRuntimeError.divisionByZero(token(DivisionByZeroName, lineNo))).ok
    )

  private def error(failure: FRuntimeError): RuntimeError = RuntimeError(failure)

  private def token(name: String, lineNo: Int): Token =
    Token(TokenType.Identifier, name, lineNo)
}
