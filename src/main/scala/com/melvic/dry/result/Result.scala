package com.melvic.dry.result

import com.melvic.dry.result.Nel.One

object Result {
  type Result[+A] = Either[Nel[Failure], A]

  def succeed[A](value: A): Result[A] =
    Right(value)

  def fail[A](error: Failure): Result[A] =
    Left(One(error))

  def failAll[A](errors: Nel[Failure]): Result[A] =
    Left(errors)

  def fromOption[A](option: Option[A], error: => Failure): Result[A] =
    option.toRight(One(error))

  object implicits {
    implicit class ToResult[A](value: A) {
      def ok: Result[A] =
        Result.succeed(value)
    }
  }
}
