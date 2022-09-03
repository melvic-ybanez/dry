package com.melvic.dry.ast

sealed trait Stmt

object Stmt {
  final case class ExprStmt(expr: Expr) extends Stmt
  final case class PrintStmt(expr: Expr) extends Stmt
}
