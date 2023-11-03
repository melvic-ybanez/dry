package com.melvic.dry.interpreter.values

import com.melvic.dry.interpreter.Env

import scala.collection.mutable

final case class DDictionary(table: mutable.Map[Value, Value], env: Env) extends DObject with Countable {
  override def klass: Metaclass = DClass("Dictionary", Map.empty, env)

  override def fields: mutable.Map[String, Value] =
    addSizeMethod(env)(Map.empty)
      .to(mutable.Map)

  override def size = table.size

  def getByKey(key: Value): Option[Value] = table.get(key)

  def setByKey(key: Value, value: Value): Value.Bool = {
    val oldSize = table.size
    table += key -> value
    Value.Bool(oldSize != table.size)
  }

  def deleteByKey(key: Value): Option[Value] =
    table.remove(key)
}
