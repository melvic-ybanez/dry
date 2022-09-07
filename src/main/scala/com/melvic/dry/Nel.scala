package com.melvic.dry

import com.melvic.dry.Nel.{Many, One}

/**
 * Non-empty list
 */
sealed trait Nel[+A] {
  def foreach(f: A => Unit): Unit =
    this match {
      case One(value) => f(value)
      case Many(head, tail) =>
        f(head)
        tail.foreach(f)
    }
}

object Nel {
  final case class One[+A](value: A)               extends Nel[A]
  final case class Many[+A](head: A, tail: Nel[A]) extends Nel[A]
}
