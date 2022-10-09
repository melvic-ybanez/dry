package com.melvic.dry

package object aux {
  type Id[A] = A
  type FCoAlgebra[F[_], A] = A => F[A]
}
