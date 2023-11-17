package com.melvic.dry.interpreter.values

import com.melvic.dry.Token
import com.melvic.dry.interpreter.Env
import com.melvic.dry.interpreter.values.Callable.{Function => DFunction}
import com.melvic.dry.interpreter.values.DClass.Methods
import com.melvic.dry.lexer.Lexemes
import com.melvic.dry.result.Result.implicits.ToResult

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

class DClass private[values] (val name: String, val methods: Methods, val enclosing: Env)
    extends Callable
    with Metaclass
    with DObject {
  override def arity = findMethod(Lexemes.Init).map(_.arity).getOrElse(0)

  override def call(token: Token) = arguments =>
    DInstance.fromClass(this).pipe { instance =>
      findMethod(Lexemes.Init)
        .fold((instance: Value).ok)(_.bind(instance).call(token)(arguments).map(_ => instance))
    }

  override def klass = Metaclass

  override val fields = mutable.Map(Attributes.Name -> Value.Str(name))
}

object DClass {
  type Methods = Map[String, DFunction]

  def apply(name: String, methods: Methods, enclosing: Env): DClass =
    new DClass(name, methods, enclosing).addField(Attributes.Class, Metaclass)

  def unapply(klass: DClass): Option[(String, Methods, Env)] =
    Some(klass.name, klass.methods, klass.enclosing)

  def default(name: String, env: Env): DClass =
    new DClass(name, Map.empty, env)
}
