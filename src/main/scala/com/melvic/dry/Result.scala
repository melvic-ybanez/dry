package com.melvic.dry

object Result {
  type Result[+A] = Either[List[Error], A]

  def success[A](value: A): Result[A] =
    Right(value)

  def fail[A](error: Error): Result[A] =
    Left(error :: Nil)

  def fromOption[A](option: Option[A], error: => Error): Result[A] =
    option.toRight(error :: Nil)

  object impilcits {
    implicit class ToResult[A](value: A) {
      def ok: Result[A] =
        Result.success(value)
    }
  }
}
