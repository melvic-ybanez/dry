package com.melvic.dry.interpreter.values

import com.melvic.dry.Token
import com.melvic.dry.interpreter.Env
import com.melvic.dry.interpreter.values.Collections.Sizeable
import com.melvic.dry.interpreter.values.Value.{Str, Types}
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result.implicits.ToResult

import scala.collection.mutable

final case class DDictionary(table: mutable.Map[Token, Value], env: Env) extends DObject with Sizeable {
  override def klass: Metaclass = DClass("Dictionary", Map.empty, env)

  override def fields: mutable.Map[String, Value] =
    addSizeMethod(env)(Map.empty)
      .to(mutable.Map)

  override def size = table.size

  def getByKey(key: Token): Option[Value] =
    table
      .find {
        case (Token(tokenType, lexeme, _), _) if tokenType == key.tokenType && lexeme == key.lexeme =>
          true
        case _ => false
      }
      .map(_._2)
}
