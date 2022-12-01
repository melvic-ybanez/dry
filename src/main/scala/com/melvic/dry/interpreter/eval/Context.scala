package com.melvic.dry.interpreter.eval

import com.melvic.dry.interpreter.Env
import com.melvic.dry.resolver.Locals

import scala.language.implicitConversions

final case class Context[+A](node: A, env: Env, locals: Locals)

object Context {
  object implicits {
    implicit def env[A](implicit context: Context[A]): Env =
      context.env

    implicit def contextFromNode[A](node: A)(implicit env: Env, locals: Locals): Context[A] =
      Context(node, env, locals)

    implicit def node[A](implicit context: Context[A]): A =
      context.node

    implicit def locals[A](implicit context: Context[A]): Locals =
      context.locals
  }
}
