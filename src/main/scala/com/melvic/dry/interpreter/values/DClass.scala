package com.melvic.dry.interpreter.values

import com.melvic.dry.interpreter.Env
import com.melvic.dry.lexer.Lexemes
import com.melvic.dry.result.Result.implicits.ToResult

import scala.util.chaining.scalaUtilChainingOps

final case class DClass(name: String, methods: Map[String, Callable.Function], enclosing: Env)
    extends Callable {
  override def arity = findMethod(Lexemes.Init).map(_.arity).getOrElse(0)

  override def call = arguments =>
    DInstance.fromClass(this).pipe { instance =>
      findMethod(Lexemes.Init).fold((instance: Value).ok)(_.bind(instance).call(arguments).map(_ => instance))
    }

  def findMethod(name: String): Option[Callable.Function] =
    methods.get(name)
}
