package com.melvic.dry.parsers

import scala.language.implicitConversions

final case class Step[+A](value: A, next: Parser) {
  def map[B](f: A => B): Step[B] =
    Step(f(value), next)

  def toParseResult: ParseResult[A] =
    ParseResult.fromStep(this)
}

object Step {

  /**
   * Converts a step to a parser. This is usually used for for-comprehensions.
   */
  implicit def stepToParser[A](step: Step[A]): Parser =
    step.next
}
