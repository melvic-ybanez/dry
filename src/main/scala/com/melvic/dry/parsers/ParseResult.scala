package com.melvic.dry.parsers

import com.melvic.dry.Result.Result
import com.melvic.dry.{Failure, Result}

import scala.util.chaining.scalaUtilChainingOps

final case class ParseResult[+A](result: Result[A], parser: Parser) {
  def map[B](f: State[A] => State[B]): ParseResult[B] =
    result match {
      case Right(value) => f(State(value, parser)).toParseResult
      case Left(errors) => ParseResult.failAll(errors, parser)
    }

  def mapValue[B](f: A => B): ParseResult[B] =
    map(state => State(f(state.value), parser))

  def flatMap[B](f: State[A] => ParseResult[B]): ParseResult[B] =
    result match {
      case Right(value) => f(State(value, parser))
      case Left(errors) => ParseResult.failAll(errors, parser)
    }

  def mapErrors(f: (List[Failure], Parser) => (List[Failure], Parser)): ParseResult[A] =
    result match {
      case Left(errors) =>
        f(errors, parser).pipe { case (newErrors, newParser) => ParseResult.failAll(newErrors, newParser) }
      case _ => this
    }

  def mapParser(f: Parser => Parser): ParseResult[A] =
    copy(parser = f(parser))
}

object ParseResult {
  def success[A](value: A, parser: Parser): ParseResult[A] =
    ParseResult(Result.success(value), parser)

  def fail[A](failure: Failure, parser: Parser): ParseResult[A] =
    ParseResult(Result.fail(failure), parser)

  def failAll[A](failures: List[Failure], parser: Parser): ParseResult[A] =
    ParseResult(Result.failAll(failures), parser)
}
