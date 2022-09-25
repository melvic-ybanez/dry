package com.melvic.dry

import com.melvic.dry.aux.HasFlatMap

object implicits {
  implicit class ListOps[A](xs: List[A]) {
    def toCsv: String =
      xs.mkString(", ")
  }

  implicit class FunctionOps[A, B](f: A => B) {
    def flatMap[C](g: B => A => C): A => C =
      a => g(f(a))(a)
  }

  implicit class EffectfulFunctionOps[F[_]: HasFlatMap, A, B, C](f: A => F[B]) {
    def >=>(g: B => F[C]): A => F[C] =
      f.andThen(h => HasFlatMap[F].flatMap(h)(g))
  }
}
