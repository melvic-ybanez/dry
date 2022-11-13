package com.melvic.dry.interpreter.values

trait Metaclass {
  def methods: Map[String, Callable.Function]

  def findMethod(name: String): Option[Callable.Function] =
    methods.get(name)
}

object Metaclass extends Metaclass {
  override def methods = Map.empty
}
