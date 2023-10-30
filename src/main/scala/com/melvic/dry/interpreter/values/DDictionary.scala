package com.melvic.dry.interpreter.values

import com.melvic.dry.interpreter.Env
import com.melvic.dry.interpreter.values.Collections.Sizeable
import com.melvic.dry.interpreter.values.Value.{Str, Types}
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result.implicits.ToResult

import scala.collection.mutable

final case class DDictionary(table: mutable.Map[Value, Value], env: Env) extends DObject with Sizeable {
  override def klass: Metaclass = DClass("Dictionary", Map.empty, env)

  override def fields: mutable.Map[String, Value] =
    addSizeMethod(env)
      .andThen(addAddMethod)(Map.empty)
      .to(mutable.Map)

  override def size = table.size

  private def addAddMethod: AddProperty =
    _ + ("add" -> Callable.withLineNo(2, env)(line => {
      case (key @ Str(_)) :: value :: _ => copy(table = table += (key -> value)).ok
      case arg :: _ => RuntimeError.invalidArgument(Types.String, Value.typeOf(arg), line).fail
    }))
}
