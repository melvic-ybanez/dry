package com.melvic.dry.interpreter.values

import scala.collection.mutable

final case class DInstance private (klass: Metaclass, fields: mutable.Map[String, Value]) extends DObject

object DInstance {
  def fromClass(klass: DClass): DInstance = DInstance(klass, mutable.Map.empty)
}
