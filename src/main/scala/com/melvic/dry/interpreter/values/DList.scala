package com.melvic.dry.interpreter.values

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Expr.{Lambda, Literal}
import com.melvic.dry.ast.Stmt.ReturnStmt
import com.melvic.dry.interpreter.Env

import scala.collection.mutable

final case class DList(elems: List[Value], env: Env) extends DObject {
  override def klass: Metaclass = DClass("List", Map.empty, env)

  override val fields: mutable.Map[String, Value] =
    elems.zipWithIndex
      .map { case (elem, i) => ("_" + i) -> elem }
      .to(mutable.Map)
      .addOne(
        "size",
        Callable.Lambda(
          Lambda(Nil, ReturnStmt(Token(TokenType.Return, "return", 0), Literal.Number(elems.size)) :: Nil),
          env
        )
      )
}
