package com.melvic.dry.ast

import com.melvic.dry.ast.Decl.StmtDecl
import com.melvic.dry.ast.Stmt.Loop.While
import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.lexer.Lexemes
import com.melvic.dry.{Show, Token}

sealed trait Stmt extends Decl

object Stmt {
  final case class ExprStmt(expr: Expr) extends Stmt

  final case class BlockStmt(declarations: List[Decl]) extends Stmt {
    def append(decl: Decl): BlockStmt =
      BlockStmt(declarations :+ decl)
  }

  object BlockStmt {
    def fromDecls(declarations: Decl*): BlockStmt =
      BlockStmt(declarations.toList)

    def fromStmts(stmts: Stmt*): BlockStmt =
      fromDecls(stmts.map(StmtDecl(_)): _*)

    def append(block: Stmt, child: Decl): BlockStmt =
      block match {
        case blockStmt: BlockStmt => blockStmt.append(child)
        case _                    => BlockStmt.fromDecls(StmtDecl(block), child)
      }

    def show: Show[BlockStmt] = block => s"{ ${block.declarations.map(Decl.show).mkString(" ")} }"
  }

  sealed trait IfStmt extends Stmt {
    def condition: Expr
  }

  object IfStmt {
    final case class IfThen(condition: Expr, thenBranch: Stmt) extends IfStmt
    final case class IfThenElse(condition: Expr, thenBranch: Stmt, elseBranch: Stmt) extends IfStmt

    def show: Show[IfStmt] = {
      case IfThen(condition, thenBranch) =>
        s"if (${Expr.show(condition)}) ${BlockStmt.show(BlockStmt.fromStmts(thenBranch))}"
      case IfThenElse(condition, thenBranch, elseBranch) =>
        IfStmt.show(IfThen(condition, thenBranch)) + " else " + BlockStmt.show(
          BlockStmt.fromStmts(elseBranch)
        )
    }
  }

  object Loop {
    final case class While(condition: Expr, body: Stmt) extends Stmt
  }

  final case class ReturnStmt(keyword: Token, value: Expr) extends Stmt

  final case class Import(path: List[Token]) extends Stmt {
    def name: Token = path.last
  }

  def show: Show[Stmt] = {
    case ExprStmt(expr)       => show"$expr;"
    case blockStmt: BlockStmt => BlockStmt.show(blockStmt)
    case ifStmt: IfStmt       => IfStmt.show(ifStmt)
    // Note: For loops desugar to while loops, so printing a stringified for loop invokes this
    case While(condition, body) => show"${Lexemes.While} ($condition) $body"
    case ReturnStmt(_, value)   => show"${Lexemes.Return} $value;"
    case Import(path)           => show"${Lexemes.Import} ${path.map(Token.show).mkString(".")};"
  }
}
