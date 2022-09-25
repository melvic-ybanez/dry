package com.melvic.dry.parsers

import com.melvic.dry.aux.Nel
import com.melvic.dry.result.Result.Result
import com.melvic.dry.result.{Failure, Result}

import scala.util.chaining.scalaUtilChainingOps

/**
 * Represents the result of a parsing function. It always includes a parser. If the operation is a failure, we
 * can use the parser field to proceed with the parsing, ideally after synchronization (by invoking
 * [[Parser.synchronize]]). This way we can accumulate as many errors as possible.
 */
final case class ParseResult[+A](result: Result[A], parser: Parser) {
  def map[B](f: Step[A] => Step[B]): ParseResult[B] =
    fold[ParseResult[B]](ParseResult.failAll)(step => ParseResult.fromStep(f(step)))

  def mapValue[B](f: A => B): ParseResult[B] =
    map(step => Step(f(step.value), parser))

  def flatMap[B](f: Step[A] => ParseResult[B]): ParseResult[B] =
    fold[ParseResult[B]](ParseResult.failAll)(f)

  def leftMap(f: (Nel[Failure], Parser) => (Nel[Failure], Parser)): ParseResult[A] =
    result match {
      case Left(errors) =>
        f(errors, parser).pipe { case (newErrors, newParser) => ParseResult.failAll(newErrors, newParser) }
      case _ => this
    }

  def mapParser(f: Parser => Parser): ParseResult[A] =
    copy(parser = f(parser))

  def flatMapParser(f: Parser => ParseResult[_]): ParseResult[A] =
    f(parser).copy(result = result)

  def combineErrors(moreErrors: Nel[Failure], newParser: Parser): ParseResult[A] =
    leftMap((errors, _) => (errors ++ moreErrors, newParser))
      // make sure to convert a successful result to a failing one
      .flatMap(_ => ParseResult.failAll(moreErrors, newParser))


  def fold[B](ifError: (Nel[Failure], Parser) => B)(ifSuccess: Step[A] => B): B =
    result match {
      case Left(errors) => ifError(errors, parser)
      case Right(value) => ifSuccess(Step(value, parser))
    }
}

object ParseResult {
  def fromStep[A](step: Step[A]): ParseResult[A] =
    succeed(step.value, step.next)

  def succeed[A](value: A, parser: Parser): ParseResult[A] =
    ParseResult(Result.succeed(value), parser)

  def fail[A](failure: Failure, parser: Parser): ParseResult[A] =
    ParseResult(Result.fail(failure), parser)

  def failAll[A](failures: Nel[Failure], parser: Parser): ParseResult[A] =
    ParseResult(Result.failAll(failures), parser)
}
