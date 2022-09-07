package com.melvic.dry

object Result {
  // TODO: Left channel can't be empty
  type Result[+A] = Either[List[Failure], A]

  def success[A](value: A): Result[A] =
    Right(value)

  def fail[A](error: Failure): Result[A] =
    Left(error :: Nil)

  def failAll[A](errors: List[Failure]): Result[A] =
    Left(errors)

  def fromOption[A](option: Option[A], error: => Failure): Result[A] =
    option.toRight(error :: Nil)

  object impilcits {
    implicit class ToResult[A](value: A) {
      def ok: Result[A] =
        Result.success(value)
    }
  }
}
