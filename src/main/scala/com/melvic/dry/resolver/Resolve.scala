package com.melvic.dry.resolver

import com.melvic.dry.Token
import com.melvic.dry.ast.Decl.Let.{LetDecl, LetInit}
import com.melvic.dry.ast.Decl.{Def, StmtDecl}
import com.melvic.dry.ast.Expr._
import com.melvic.dry.ast.Stmt.IfStmt.{IfThen, IfThenElse}
import com.melvic.dry.ast.Stmt.Loop.While
import com.melvic.dry.ast.Stmt.{BlockStmt, ExprStmt, ReturnStmt}
import com.melvic.dry.ast.{Decl, Expr, Stmt}
import com.melvic.dry.aux.HasFlatMap._
import com.melvic.dry.aux.implicits._
import com.melvic.dry.resolver.ScopesFunction._
import com.melvic.dry.result.Failure
import com.melvic.dry.result.Result.implicits.ToResult

object Resolve {
  def blockStmt: BlockStmt => Resolve = block =>
    Scopes.start.ok >=> Resolve.decls(block.declarations) >=> Scopes.end.ok

  def decls: List[Decl] => Resolve = decls =>
    context => decls.foldFailFast(context.ok)((context, decl) => Resolve.decl(decl)(context))

  def decl: Decl => Resolve = {
    case LetDecl(name) => Scopes.declare(name).ok >=> Scopes.define(name).ok
    case LetInit(name, init) =>
      Scopes.declare(name).ok >=> Resolve.expr(init) >=> Scopes.define(name).ok
    case function: Def  => Resolve.function(function)
    case StmtDecl(stmt) => Resolve.stmt(stmt)
    case stmt: Stmt     => Resolve.stmt(stmt)
  }

  def stmt: Stmt => Resolve = {
    case ExprStmt(expr)            => Resolve.expr(expr)
    case IfThen(condition, branch) => Resolve.expr(condition) >=> Resolve.stmt(branch)
    case IfThenElse(condition, thenBranch, elseBranch) =>
      Resolve.expr(condition) >=> Resolve.stmt(thenBranch) >=> Resolve.stmt(elseBranch)
    case ReturnStmt(Literal.None) => _.ok
    case ReturnStmt(value)        => Resolve.expr(value)
    case While(condition, body)   => Resolve.expr(condition) >=> Resolve.stmt(body)
    case blockStmt: BlockStmt     => Resolve.blockStmt(blockStmt)
  }

  def expr: Expr => Resolve = {
    case _: Literal              => _.ok
    case Unary(_, operand)       => Resolve.expr(operand)
    case Binary(left, _, right)  => Resolve.expr(left) >=> Resolve.expr(right)
    case Logical(left, _, right) => Resolve.expr(left) >=> Resolve.expr(right)
    case Grouping(expr)          => Resolve.expr(expr)
    case expr @ Variable(name) =>
      Scopes.resolveFromHead { scope =>
        scope
          .get(name.lexeme)
          .map { found =>
            if (found) Resolve.local(name)(expr)
            else
              ScopesFunction.fail(
                Failure.resolution(name.line, s"${name.lexeme} is declared but not yet defined")
              )
          }
          .getOrElse(Resolve.local(name)(expr))
      }
    case expr @ Assignment(name, value) => Resolve.expr(value) >=> Resolve.local(name)(expr)
    case Call(callee, arguments, _) =>
      Resolve.expr(callee) >=> { scopes =>
        arguments.foldLeft(scopes.ok)((acc, arg) => acc.flatMap(Resolve.expr(arg)))
      }
    case lambda: Lambda => Resolve.lambda(lambda)
  }

  def function: Def => Resolve = { case Def(name, params, body) =>
    Scopes.declare(name).ok >=> Scopes.define(name).ok >=> Resolve.lambda(Lambda(params, body))
  }

  def lambda: Lambda => Resolve = { case Lambda(params, body) =>
    Scopes.start.ok >=> { contexts =>
      params.foldFailFast(contexts.ok) { (contexts, param) =>
        (Scopes.declare(param).ok >=> Scopes.define(param).ok)(contexts)
      }
    } >=> Resolve.blockStmt(BlockStmt(body)) >=> Scopes.end.ok
  }

  def exprWithDepth(depth: Int): Expr => Resolve = expr => { case (scopes, locals) =>
    (scopes, locals + (LocalExprKey(expr) -> depth)).ok
  }

  def local(name: Token): Expr => Resolve = { expr =>
    { case (scopes, locals) =>
      val maybeFound = scopes.zipWithIndex.find { case (scope, _) =>
        scope.contains(name.lexeme)
      }
      maybeFound
        .map { case (_, i) => Resolve.exprWithDepth(i)(expr)(scopes, locals) }
        .getOrElse((scopes, locals).ok)
    }
  }
}
