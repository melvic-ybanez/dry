package com.melvic.dry.resolver

import com.melvic.dry.ast.Expr

class LocalExprKey(val expr: Expr) {
  override def equals(obj: Any): Boolean =
    obj match {
      case that: LocalExprKey =>
        this.expr eq that.expr
      case _ => false
    }

  override def hashCode(): Int = expr.hashCode()
}

object LocalExprKey {
  def apply(expr: Expr): LocalExprKey = new LocalExprKey(expr)
}
