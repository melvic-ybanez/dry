package com.melvic.dry

import com.melvic.dry.result.Result.Result

trait HasFlatMap[F[_]] {
  def flatMap[A, B](instance: F[A])(f: A => F[B]): F[B]
}

object HasFlatMap {
  def apply[F[_]](implicit hasFlatMap: HasFlatMap[F]): HasFlatMap[F] =
    hasFlatMap

  implicit val resultHasFlatMap: HasFlatMap[Result] =
    new HasFlatMap[Result] {
      override def flatMap[A, B](instance: Result[A])(f: A => Result[B]) =
        instance.flatMap(f)
    }
}
