package com.melvic.dry

object implicits {
  implicit class ListOps[A](xs: List[A]) {
    def toCsv: String =
      xs.mkString(", ")
  }

  implicit class FunctionOps[A, B](f: A => B) {
    def map[C](g: B => C): A => C =
      g compose f

    def flatMap[C](g: B => A => C): A => C =
      a => g(f(a))(a)
  }
}
