package com.melvic.dry

object Result {
  type Result[+A] = Either[Error, A]

  def success[A](value: A): Result[A] =
    Right(value)

  def fail[A](error: Error): Result[A] =
    Left(error)

  def fromOption[A](option: Option[A], error: => Error): Result[A] =
    option.toRight(error)

  object impilcits {
    implicit class ResultOps[A](value: A) {
      def ok: Result[A] =
        Result.success(value)
    }
  }
}
