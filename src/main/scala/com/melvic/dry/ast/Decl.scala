package com.melvic.dry.ast

import com.melvic.dry.ast.Stmt.{BlockStmt, ExprStmt}
import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.aux.implicits.ListOps
import com.melvic.dry.lexer.Lexemes
import com.melvic.dry.{Show, Token}

//noinspection SpellCheckingInspection
trait Decl

//noinspection SpellCheckingInspection
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

  final case class ClassDecl(name: Token, methods: List[Def]) extends Decl

  object ClassDecl {
    def show: Show[ClassDecl] = {
      case ClassDecl(name, methods) =>
        show"${Lexemes.Class} $name ${BlockStmt.fromDecls(methods: _*)}"
    }
  }

  def show: Show[Decl] = {
    case let: Let           => Let.show(let)
    case stmtDecl: StmtDecl => StmtDecl.show(stmtDecl)
    case Def(name, params, body) =>
      s"${Lexemes.Def} $name(${params.map(Token.show).toCsv}) ${BlockStmt.fromDecls(body: _*)}"
    case classDecl: ClassDecl => ClassDecl.show(classDecl)
    case stmt: Stmt => Stmt.show(stmt)
  }
}
