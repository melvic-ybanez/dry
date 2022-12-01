package com.melvic.dry.interpreter.eval

import com.melvic.dry.interpreter.Env
import com.melvic.dry.resolver.Locals

import java.nio.file.Path
import scala.language.implicitConversions

final case class Context[+A](node: A, env: Env, locals: Locals, sourcePaths: List[Path])

object Context {
  object implicits {
    implicit def env[A](implicit context: Context[A]): Env =
      context.env

    implicit def contextFromNode[A](node: A)(implicit
        env: Env,
        locals: Locals,
        sources: List[Path]
    ): Context[A] =
      Context(node, env, locals, sources)

    implicit def node[A](implicit context: Context[A]): A =
      context.node

    implicit def locals[A](implicit context: Context[A]): Locals =
      context.locals

    implicit def sourcePaths[A](implicit context: Context[A]): List[Path] =
      context.sourcePaths
  }
}
