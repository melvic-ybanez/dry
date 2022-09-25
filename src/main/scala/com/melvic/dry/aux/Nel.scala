package com.melvic.dry.aux

import com.melvic.dry.aux.Nel.{Many, One}

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

  def ::[B >: A](value: B): Nel[B] = Many(value, this)

  def ++[B >: A](that: Nel[B]): Nel[B] =
    this match {
      case One(head)        => head :: that
      case Many(head, tail) => head :: (tail ++ that)
    }

  def length: Int =
    this match {
      case One(_)        => 1
      case Many(_, tail) => 1 + tail.length
    }
}

object Nel {
  final case class One[+A](value: A) extends Nel[A]
  final case class Many[+A](head: A, tail: Nel[A]) extends Nel[A]
}
