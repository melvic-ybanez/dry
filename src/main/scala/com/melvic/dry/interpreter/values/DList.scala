package com.melvic.dry.interpreter.values

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Expr.{Lambda, Literal}
import com.melvic.dry.ast.Stmt.ReturnStmt
import com.melvic.dry.interpreter.Env
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

final case class DList(elems: List[Value], env: Env) extends DObject {
  type AddProperties = Map[String, Value] => Map[String, Value]

  override def klass: Metaclass = DClass("List", Map.empty, env)

  override val fields: mutable.Map[String, Value] =
    addIndexFields
      .pipe(addAtMethod)
      .pipe(addSizeMethod)
      .to(mutable.Map)

  private def addIndexFields =
    elems.zipWithIndex
      .map { case (elem, i) => ("_" + i) -> elem }
      .to(Map)

  private def addAtMethod: AddProperties = {
    lazy val error = RuntimeError.indexOutOfBounds(lineNumber)
    _ + ("at" -> Callable.unary(env)(elem =>
      Result.fromOption(elem.toNum.flatMap(e => elems.lift(e.value.toInt)), error)
    ))
  }

  private def addSizeMethod: AddProperties =
    _ + ("size" -> Callable.Lambda(
      Lambda(
        Nil,
        ReturnStmt(Token(TokenType.Return, "return", lineNumber), Literal.Number(elems.size)) :: Nil
      ),
      env
    ))

  private def lineNumber: Int =
    env.at(0, Callable.LineNumber).flatMap(_.toNum.map(_.value.toInt)).getOrElse(0)
}
