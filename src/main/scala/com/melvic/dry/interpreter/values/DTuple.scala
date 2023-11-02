package com.melvic.dry.interpreter.values
import com.melvic.dry.interpreter.Env
import com.melvic.dry.interpreter.values.Collections.Countable

import scala.collection.mutable

final case class DTuple(elems: List[Value], env: Env) extends DObject with Countable {
  override def klass: Metaclass = DClass("Tuple", Map.empty, env)

  override def fields: mutable.Map[String, Value] =
    addSizeMethod(env)(Map.empty)
      .to(mutable.Map)

  override def size: Int = elems.size
}

