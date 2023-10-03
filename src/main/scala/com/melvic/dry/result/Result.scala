package com.melvic.dry.result

import com.melvic.dry.aux.Nel
import com.melvic.dry.aux.Nel.One
import com.melvic.dry.interpreter.Env

object Result {
  type Result[+A] = Either[Nel[Failure], A]
  type ResultFrom[A] = A => Result[A]

  def succeed[A](value: A): Result[A] =
    Right(value)

  def fail[A](error: Failure): Result[A] =
    Left(One(error))

  def failAll[A](errors: Nel[Failure]): Result[A] =
    Left(errors)

  def fromOption[A](option: Option[A], error: => Failure): Result[A] =
    option.toRight(One(error))

  def failFromEnv[A](failure: Failure): Env => Result[A] =
    _ => Result.fail(failure)

  def succeedFromEnv[A](value: A): Env => Result[A] =
    _ => Result.succeed(value)

  def foreachFailure[A](result: Result[A])(f: Failure => Unit): Unit =
    result.left.foreach(_.foreach(f))

  object implicits {
    implicit class ToResult[A](value: A) {
      def ok: Result[A] =
        Result.succeed(value)
    }
  }
}
