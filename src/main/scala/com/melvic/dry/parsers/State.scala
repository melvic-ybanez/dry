package com.melvic.dry.parsers

final case class State[+A](value: A, parser: Parser) {
  def map[B](f: A => B): State[B] =
    State(f(value), parser)

  def toParseResult: ParseResult[A] =
    ParseResult.succeed(value, parser)
}
