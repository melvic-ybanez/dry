package com.melvic.dry.interpreter.values

import com.melvic.dry.interpreter.Env
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.implicits.ToResult

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.chaining.scalaUtilChainingOps

final case class DList(elems: ListBuffer[Value], env: Env) extends DObject {
  type AddProperties = Map[String, Value] => Map[String, Value]

  override def klass: Metaclass = DClass("List", Map.empty, env)

  override val fields: mutable.Map[String, Value] =
    addIndexFields
      .pipe(addAtMethod)
      .pipe(addSizeMethod)
      .pipe(addAddMethod)
      .to(mutable.Map)

  private def addIndexFields =
    elems.zipWithIndex
      .map { case (elem, i) => ("_" + i) -> elem }
      .to(Map)

  private def addAtMethod: AddProperties =
    _ + ("at" -> Callable.withLineNo(1, env)(line => { case indexValue :: _ =>
      indexValue.toNum
        .fold(RuntimeError.invalidIndex(Value.show(indexValue), line).fail[Value]) { num =>
          val index = num.value.toInt
          Result.fromOption(elems.lift(index), RuntimeError.indexOutOfBounds(index, line))
        }
    }))

  private def addSizeMethod: AddProperties =
    _ + ("size" -> Callable.noArg(env) { Value.Num(elems.size.toInt).ok })

  private def addAddMethod: AddProperties =
    _ + ("add" -> Callable.unarySuccess(env)(elem => copy(elems = elems += elem)))
}
