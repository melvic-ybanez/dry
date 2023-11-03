package com.melvic.dry.interpreter.values

import com.melvic.dry.interpreter.Env
import com.melvic.dry.interpreter.values.Value.ToValue

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final case class DList(elems: ListBuffer[Value], env: Env) extends DSequence {
  override def klass: Metaclass = DClass("List", Map.empty, env)

  override val fields: mutable.Map[String, Value] =
    addSizeMethod(env)
      .andThen(addAddMethod)(Map.empty)
      .to(mutable.Map)

  private def addAddMethod: AddProperty =
    _ + ("add" -> Callable.unarySuccess(env)(elem => copy(elems = elems += elem)))

  override def size: Int = elems.size

  override def getByIndex(index: Int): Value = elems(index)

  def setByIndex(index: Int, value: Value): Value =
    (elems(index) = value).unit

  def deleteByIndex(index: Int): Value =
    elems.remove(index).unit
}
