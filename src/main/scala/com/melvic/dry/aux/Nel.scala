package com.melvic.dry.aux

import com.melvic.dry.aux.Nel.{Many, One}

import scala.annotation.tailrec

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

  def foldLeft[B](init: B)(f: (B, A) => B): B = {
    @tailrec
    def recurse(acc: B, rest: Nel[A]): B =
      rest match {
        case One(value)       => f(acc, value)
        case Many(head, tail) => recurse(f(acc, head), tail)
      }

    recurse(init, this)
  }

  def toList: List[A] =
    foldLeft(List.empty[A]) { (acc, value) => value :: acc }.reverse

  def length: Int =
    foldLeft(0) { (sum, _) => sum + 1 }

  def reverse: Nel[A] =
    this match {
      case One(_) => this
      case Many(head, tail) =>
        tail.foldLeft(One(head): Nel[A])((acc, value) => value :: acc)
    }
}

object Nel {
  final case class One[+A](value: A) extends Nel[A]
  final case class Many[+A](head: A, tail: Nel[A]) extends Nel[A]
}
