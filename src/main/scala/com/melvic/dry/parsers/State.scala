package com.melvic.dry.parsers

import scala.language.implicitConversions

final case class State[+A](value: A, parser: Parser) {
  def map[B](f: A => B): State[B] =
    State(f(value), parser)

  def toParseResult: ParseResult[A] =
    ParseResult.succeed(value, parser)
}

object State {

  /**
   * Converts a state to a parser. This is usually used for for-comprehensions.
   */
  implicit def stateToParser[A](state: State[A]): Parser =
    state.parser
}
