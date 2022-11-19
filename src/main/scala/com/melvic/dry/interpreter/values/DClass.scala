package com.melvic.dry.interpreter.values

import com.melvic.dry.interpreter.Env
import com.melvic.dry.interpreter.values.Callable.{Function => DFunction}
import com.melvic.dry.lexer.Lexemes
import com.melvic.dry.result.Result.implicits.ToResult

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

final case class DClass(name: String, methods: Map[String, DFunction], enclosing: Env)
    extends Callable
    with Metaclass
    with DObject {
  override def arity = findMethod(Lexemes.Init).map(_.arity).getOrElse(0)

  override def call = arguments =>
    DInstance.fromClass(this).pipe { instance =>
      findMethod(Lexemes.Init)
        .fold((instance: Value).ok)(_.bind(instance).call(arguments).map(_ => instance))
    }

  override def klass = Metaclass

  override val fields = mutable.Map.empty
}
