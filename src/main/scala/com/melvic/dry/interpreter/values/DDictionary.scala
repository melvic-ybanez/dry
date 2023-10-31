package com.melvic.dry.interpreter.values

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.interpreter.Env
import com.melvic.dry.interpreter.values.Collections.Sizeable

import scala.collection.mutable

final case class DDictionary(table: mutable.Map[(TokenType, String), Value], env: Env)
    extends DObject
    with Sizeable {
  override def klass: Metaclass = DClass("Dictionary", Map.empty, env)

  override def fields: mutable.Map[String, Value] =
    addSizeMethod(env)(Map.empty)
      .to(mutable.Map)

  override def size = table.size

  def getByKey(key: Token): Option[Value] = table.get(key.tokenType, key.lexeme)

  def setByKey(key: Token, value: Value): Value.Bool = {
    val oldSize = table.size
    table += (key.tokenType, key.lexeme) -> value
    Value.Bool(oldSize != table.size)
  }
}
