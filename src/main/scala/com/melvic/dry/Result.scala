package com.melvic.dry

object Result {
  type Result[+A] = Either[Error, A]

  def success[A](value: A): Result[A] =
    Right(value)

  def fail[A](error: Error): Result[A] =
    Left(error)
}
