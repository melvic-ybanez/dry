package com.melvic.dry.interpreter.values

import com.melvic.dry.interpreter.Env
import com.melvic.dry.result.Result.implicits.ToResult

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final case class DDictionary(table: mutable.Map[Value, Value], env: Env) extends DObject with Countable {
  override def klass: Metaclass = DClass("Dictionary", Map.empty, env)

  override def fields: mutable.Map[String, Value] =
    addSizeMethod(env)
      .andThen(addFieldsMethod())
      .andThen(addKeysMethod())
      .andThen(addValuesMethod())(Map.empty)
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

  private def addFieldsMethod(): AddProperty =
    fieldsComponent("fields")((key, value) => DTuple(key :: value :: Nil, env))

  private def addKeysMethod(): AddProperty = fieldsComponent("keys")((key, _) => key)

  private def addValuesMethod(): AddProperty = fieldsComponent("values")((_, value) => value)

  private def fieldsComponent(methodName: String)(f: (Value, Value) => Value): AddProperty =
    _ + (methodName -> Callable.noArg(env) {
      DList(table.map { case (key, value) => f(key, value) }.to(ListBuffer), env).ok
    })

}
