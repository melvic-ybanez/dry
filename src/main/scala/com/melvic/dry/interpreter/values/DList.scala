package com.melvic.dry.interpreter.values

import com.melvic.dry.interpreter.Env
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.chaining.scalaUtilChainingOps

final case class DList(elems: ListBuffer[Value], env: Env) extends DObject with Collections.Countable {
  override def klass: Metaclass = DClass("List", Map.empty, env)

  override val fields: mutable.Map[String, Value] =
    addIndexFields()
      .pipe(addAtMethod)
      .pipe(addSizeMethod(env))
      .pipe(addAddMethod)
      .to(mutable.Map)

  // TODO: Replace with bracket-notation
  private def addIndexFields() =
    elems.zipWithIndex
      .map { case (elem, i) => ("_" + i) -> elem }
      .to(Map)

  // TODO: Replace with bracket-notation
  private def addAtMethod: AddProperty =
    _ + ("at" -> Callable.withLineNo(1, env)(line => { case indexValue :: _ =>
      indexValue.toNum
        .fold(RuntimeError.invalidIndex(Value.show(indexValue), line).fail[Value]) { num =>
          val index = num.value.toInt
          Result.fromOption(elems.lift(index), RuntimeError.indexOutOfBounds(index, line))
        }
    }))

  private def addAddMethod: AddProperty =
    _ + ("add" -> Callable.unarySuccess(env)(elem => copy(elems = elems += elem)))

  override def size: Int = elems.size
}
