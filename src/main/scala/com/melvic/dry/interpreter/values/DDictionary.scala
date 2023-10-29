package com.melvic.dry.interpreter.values
import com.melvic.dry.interpreter.Env

import scala.collection.mutable

final case class DDictionary(table: Map[String, Value], env: Env) extends DObject {
  override def klass: Metaclass = DClass("Dictionary", Map.empty, env)

  override def fields: mutable.Map[String, Value] = mutable.Map.empty
}
