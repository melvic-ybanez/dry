package com.melvic.dry.interpreter.values

import scala.collection.mutable

/**
 * A [[DObject]] that is not a [[DClass]] and not a [[DList]]. Remember that classes in Dry are also objects.
 */
final case class DInstance private (klass: Metaclass, fields: mutable.Map[String, Value]) extends DObject

object DInstance {
  def fromClass(klass: DClass): DInstance = DInstance(klass, mutable.Map.empty)
}
