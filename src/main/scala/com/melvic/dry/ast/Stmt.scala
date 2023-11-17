package com.melvic.dry.ast

import com.melvic.dry.ast.Decl.StmtDecl
import com.melvic.dry.ast.Expr.Variable
import com.melvic.dry.ast.Stmt.Loop.While
import com.melvic.dry.aux.{Nel, Show}
import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.lexer.Lexemes
import com.melvic.dry.{Show, Token}

sealed trait Stmt extends Decl

//noinspection SpellCheckingInspection
object Stmt {
  final case class ExprStmt(expr: Expr) extends Stmt

  final case class BlockStmt(declarations: List[Decl]) extends Stmt {
    def append(decl: Decl): BlockStmt =
      BlockStmt(declarations :+ decl)
  }

  object BlockStmt {
    def fromDecls(declarations: Decl*): BlockStmt =
      BlockStmt(declarations.toList)

    def fromStatements(stmts: Stmt*): BlockStmt =
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
        s"if (${Expr.show(condition)}) ${BlockStmt.show(BlockStmt.fromStatements(thenBranch))}"
      case IfThenElse(condition, thenBranch, elseBranch) =>
        IfStmt.show(IfThen(condition, thenBranch)) + " else " + BlockStmt.show(
          BlockStmt.fromStatements(elseBranch)
        )
    }
  }

  object Loop {
    final case class While(condition: Expr, body: Stmt) extends Stmt
  }

  final case class ReturnStmt(keyword: Token, value: Expr) extends Stmt
  final case class DeleteStmt(obj: Expr, key: Expr, token: Token) extends Stmt

  final case class TryCatch(tryBlock: BlockStmt, catchBlocks: Nel[CatchBlock]) extends Stmt

  object TryCatch {
    def show: Show[TryCatch] = { case TryCatch(tryBlock, catchBlocks) =>
      show"${Lexemes.Try} $tryBlock ${Show.list(catchBlocks.toList.map(CatchBlock.show))}"
    }
  }

  sealed trait CatchBlock

  object CatchBlock {
    final case class CatchType(kind: Variable, block: BlockStmt, paren: Token) extends CatchBlock
    final case class CatchUntypedVar(instance: Variable, block: BlockStmt, paren: Token) extends CatchBlock
    final case class CatchTypedVar(instance: Variable, kind: Variable, blockStmt: BlockStmt, paren: Token)
        extends CatchBlock
    final case class CatchAll(blockStmt: BlockStmt, paren: Token) extends CatchBlock

    implicit val implicitShow: Show[CatchBlock] = show

    def show: Show[CatchBlock] = {
      def show(insideParens: String, block: BlockStmt): String =
        show"${Lexemes.Catch} ($insideParens) $block"

      {
        case CatchUntypedVar(instance, block, _)     => show(show"$instance:", block)
        case CatchType(kind, block, _)               => show(show": $kind", block)
        case CatchTypedVar(instance, kind, block, _) => show(show"$instance: $kind", block)
        case CatchAll(block, _)                      => show(":", block)
      }
    }
  }

  final case class Import(path: List[Token]) extends Stmt {
    def name: Token = path.last
  }

  implicit val implicitShow: Show[Stmt] = show

  def show: Show[Stmt] = {
    case ExprStmt(expr)       => show"$expr;"
    case blockStmt: BlockStmt => BlockStmt.show(blockStmt)
    case ifStmt: IfStmt       => IfStmt.show(ifStmt)
    // Note: For loops desugar to while loops, so printing a stringified for loop invokes this
    case While(condition, body)  => show"${Lexemes.While} ($condition) $body"
    case ReturnStmt(_, value)    => show"${Lexemes.Return} $value;"
    case DeleteStmt(obj, key, _) => show"${Lexemes.Delete} $obj[$key]"
    case tryCatch: TryCatch      => tryCatch
    case Import(path)            => show"${Lexemes.Import} ${path.map(Token.show).mkString(".")};"
  }
}
