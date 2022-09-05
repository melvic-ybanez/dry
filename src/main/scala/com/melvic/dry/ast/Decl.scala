package com.melvic.dry.ast

import com.melvic.dry.Token

trait Decl

object Decl {
  sealed trait Let extends Decl
  final case class LetInit(name: Token, init: Expr) extends Let
  final case class LetDecl(name: Token) extends Let

  final case class StmtDecl(stmt: Stmt) extends Decl
}
