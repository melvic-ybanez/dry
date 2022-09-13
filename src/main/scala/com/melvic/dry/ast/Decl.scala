package com.melvic.dry.ast

import com.melvic.dry.Token
import com.melvic.dry.ast.Stmt.ExprStmt

trait Decl

object Decl {
  sealed trait Let extends Decl
  final case class LetInit(name: Token, init: Expr) extends Let
  final case class LetDecl(name: Token) extends Let

  final case class StmtDecl(stmt: Stmt) extends Decl

  object StmtDecl {
    def fromExpr(expr: Expr): StmtDecl =
      StmtDecl(ExprStmt(expr))
  }
}
