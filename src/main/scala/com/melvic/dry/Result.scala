package com.melvic.dry

import com.melvic.dry.Nel.One

object Result {
  type Result[+A] = Either[Nel[Failure], A]

  def success[A](value: A): Result[A] =
    Right(value)

  def fail[A](error: Failure): Result[A] =
    Left(One(error))

  def failAll[A](errors: Nel[Failure]): Result[A] =
    Left(errors)

  def fromOption[A](option: Option[A], error: => Failure): Result[A] =
    option.toRight(One(error))

  object impilcits {
    implicit class ToResult[A](value: A) {
      def ok: Result[A] =
        Result.success(value)
    }
  }
}
