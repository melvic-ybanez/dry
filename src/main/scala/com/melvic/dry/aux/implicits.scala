package com.melvic.dry.aux

object implicits {
  implicit class ListOps[A](xs: List[A]) {
    def toCsv: String =
      xs.mkString(", ")

    def foldFailFast[F[_]: HasFlatMap, B](result: F[B])(f: (B, A) => F[B]): F[B] = {
      def recurse(result: F[B], xs: List[A]): F[B] =
        xs match {
          case Nil       => result
          case x :: rest => HasFlatMap[F].flatMap(result)(b => recurse(f(b, x), rest))
        }

      recurse(result, xs)
    }
  }

  implicit class FunctionOps[A, B](f: A => B) {
    def flatMap[C](g: B => A => C): A => C =
      a => g(f(a))(a)

    def contraMap[C](g: C => A): C => B =
      g.andThen(f)
  }

  implicit class KleisliOps[F[_]: HasFlatMap, A, B, C](f: A => F[B]) {
    def >=>(g: B => F[C]): A => F[C] =
      f.andThen(fb => HasFlatMap[F].flatMap(fb)(g))
  }
}
