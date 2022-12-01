package com.melvic.dry.interpreter.eval

import com.melvic.dry.interpreter.Env

import scala.language.implicitConversions

final case class Context[+A](node: A, env: Env) {
  def withNode[B](newNode: B): Context[B] =
    copy(node = newNode)
}

object Context {
  object implicits {
    implicit def env[A](implicit context: Context[A]): Env =
      context.env

    implicit def contextFromNode[A](node: A)(implicit env: Env): Context[A] =
      Context(node, env)

    implicit def node[A](implicit context: Context[A]): A =
      context.node
  }
}
