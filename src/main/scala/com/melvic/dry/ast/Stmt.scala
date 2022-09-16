package com.melvic.dry.ast

sealed trait Stmt extends Decl

object Stmt {
  final case class ExprStmt(expr: Expr) extends Stmt

  final case class BlockStmt(declarations: List[Decl]) extends Stmt

  object BlockStmt {
    def fromDecls(declarations: Decl*): BlockStmt =
      BlockStmt(declarations.toList)
  }

  sealed trait IfStmt extends Stmt {
    def condition: Expr
  }

  object IfStmt {
    final case class IfThen(condition: Expr, thenBranch: Stmt) extends IfStmt
    final case class IfThenElse(condition: Expr, thenBranch: Stmt, elseBranch: Stmt) extends IfStmt
  }

  object Loop {
    final case class While(condition: Expr, body: Stmt) extends Stmt
  }
}
