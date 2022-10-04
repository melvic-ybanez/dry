package com.melvic.dry.resolver

import com.melvic.dry.Token
import com.melvic.dry.ast.Decl.{Def, LetDecl, LetInit}
import com.melvic.dry.ast.Expr._
import com.melvic.dry.ast.Stmt.IfStmt.{IfThen, IfThenElse}
import com.melvic.dry.ast.Stmt.Loop.While
import com.melvic.dry.ast.Stmt.{BlockStmt, ExprStmt, ReturnStmt}
import com.melvic.dry.ast.{Decl, Expr, Stmt}
import com.melvic.dry.implicits._
import com.melvic.dry.result.Result.implicits.ToResult
import com.melvic.dry.result.{Failure, Result}

object Resolve {
  def blockStmt: Resolve[BlockStmt] = block =>
    Scopes.start >=> Resolve.decls(block.declarations) >=> Scopes.endScope

  def decls: Resolve[List[Decl]] = decls =>
    scopes =>
      decls.foldLeft(Result.succeed(scopes)) {
        case (result, decl) => result.flatMap(Resolve.decl(decl))
        case (scopes, _)    => scopes
      }

  def decl: Resolve[Decl] = {
    case LetDecl(name)       => Scopes.declare(name) >=> Scopes.define(name)
    case LetInit(name, init) => Scopes.declare(name) >=> Resolve.expr(init) >=> Scopes.define(name)
  }

  def stmt: Resolve[Stmt] = {
    case ExprStmt(expr)            => Resolve.expr(expr)
    case IfThen(condition, branch) => Resolve.expr(condition) >=> Resolve.stmt(branch)
    case IfThenElse(condition, thenBranch, elseBranch) =>
      Resolve.expr(condition) >=> Resolve.stmt(thenBranch) >=> Resolve.stmt(elseBranch)
    case ReturnStmt(_, Literal.None) => _.ok
    case ReturnStmt(_, value)        => Resolve.expr(value)
    case While(condition, body)      => Resolve.expr(condition) >=> Resolve.stmt(body)
  }

  def expr: Resolve[Expr] = {
    case _: Literal              => _.ok
    case Unary(_, operand)       => Resolve.expr(operand)
    case Binary(left, _, right)  => Resolve.expr(left) >=> Resolve.expr(right)
    case Logical(left, _, right) => Resolve.expr(left) >=> Resolve.expr(right)
    case Grouping(expr)          => Resolve.expr(expr)
    case expr @ Variable(name) =>
      Scopes.flatMapHead { scope =>
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
  }

  def function: Resolve[Def] = { case Def(name, params, body) =>
    Scopes.declare(name) >=> Scopes.define(name) >=> Scopes.start >=> { scopes =>
      params.foldLeft(scopes.ok)((acc, param) => acc.flatMap(Scopes.declare(param) >=> Scopes.define(param)))
    } >=> Resolve.decls(body)
  }

  def exprWithSteps(steps: Int): Resolve[Expr] = ??? // TODO: Implement this

  def local(name: Token): Resolve[Expr] = { expr => scopes =>
    val maybeFound = scopes.zipWithIndex.find { case (scope, _) =>
      scope.contains(name.lexeme)
    }
    maybeFound.map { case (_, i) => Resolve.exprWithSteps(i)(expr)(scopes) }.getOrElse(scopes.ok)
  }
}
