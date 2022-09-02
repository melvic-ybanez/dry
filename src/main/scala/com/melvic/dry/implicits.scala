package com.melvic.dry

object implicits {
  implicit class ListOps[A](xs: List[A]) {
    def toCsv: String =
      xs.mkString(", ")
  }
}
