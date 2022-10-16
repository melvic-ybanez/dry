package com.melvic.dry.ast

import com.melvic.dry.ast.Stmt.{BlockStmt, ExprStmt}
import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.aux.implicits.ListOps
import com.melvic.dry.{Show, Token}

trait Decl

object Decl {
  sealed trait Let extends Decl

  object Let {
    final case class LetInit(name: Token, init: Expr) extends Let
    final case class LetDecl(name: Token) extends Let

    def show: Show[Let] = {
      case LetInit(name, init) => show"let $name = $init;"
      case LetDecl(name)       => show"let $name;"
    }
  }

  final case class StmtDecl(stmt: Stmt) extends Decl

  object StmtDecl {
    def fromExpr(expr: Expr): StmtDecl =
      StmtDecl(ExprStmt(expr))

    def show: Show[StmtDecl] = stmtDecl => Stmt.show(stmtDecl.stmt)
  }

  final case class Def(name: Token, params: List[Token], body: List[Decl]) extends Decl

  def show: Show[Decl] = {
    case let: Let           => Let.show(let)
    case stmtDecl: StmtDecl => StmtDecl.show(stmtDecl)
    case Def(name, params, body) =>
      s"def $name(${params.map(Token.show).toCsv}) ${BlockStmt.fromDecls(body: _*)}"
    case stmt: Stmt => Stmt.show(stmt)
  }
}
