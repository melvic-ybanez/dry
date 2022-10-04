package com.melvic

package object dry {
  type Show[A] = A => String
  type Id[A] = A
}
