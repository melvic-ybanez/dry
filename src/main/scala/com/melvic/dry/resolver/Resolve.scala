package com.melvic.dry.resolver

import com.melvic.dry.Token
import com.melvic.dry.ast.Decl.Let.{LetDecl, LetInit}
import com.melvic.dry.ast.Decl.{ClassDecl, Def, StmtDecl}
import com.melvic.dry.ast.Expr._
import com.melvic.dry.ast.Stmt.IfStmt.{IfThen, IfThenElse}
import com.melvic.dry.ast.Stmt.Loop.While
import com.melvic.dry.ast.Stmt.{BlockStmt, ExprStmt, ReturnStmt}
import com.melvic.dry.ast.{Decl, Expr, Stmt}
import com.melvic.dry.aux.HasFlatMap._
import com.melvic.dry.aux.implicits._
import com.melvic.dry.resolver.ScopesFunction._
import com.melvic.dry.result.Failure.ResolutionError
import com.melvic.dry.result.Result.ResultCoAlg
import com.melvic.dry.result.Result.implicits.ToResult
import com.melvic.dry.result.{Failure, Result}

object Resolve {
  type Resolve = ResultCoAlg[Context]

  def resolveAll: List[Decl] => Resolve = decls =>
    Resolve.scanFunctions(decls).andThen(_.flatMap(Resolve.decls(decls)))

  def blockStmt: BlockStmt => Resolve = block =>
    Scopes.start.ok >=> Resolve.resolveAll(block.declarations) >=> Scopes.end.ok

  def decls: List[Decl] => Resolve = decls =>
    context => decls.foldFailFast(context.ok)((context, decl) => Resolve.decl(decl)(context))

  def scanFunctions: List[Decl] => Resolve = decls =>
    context =>
      decls.foldFailFast(context.ok) {
        case (context, Def(name, _, _))             => Scopes.declare(name).ok(context)
        case (context, LetInit(name, Lambda(_, _))) => Scopes.declare(name).ok(context)
        case (context, _)                           => context.ok
      }

  def decl: Decl => Resolve = {
    case LetDecl(name) => Scopes.declare(name).ok >=> Scopes.define(name).ok
    // lambdas stored in variables are already declared via `scanFunctions`, so there's no need to declare it again.
    case LetInit(name, init @ Lambda(_, _)) => Resolve.expr(init) >=> Scopes.define(name).ok
    case LetInit(name, init) =>
      Scopes.declare(name).ok >=> Resolve.expr(init) >=> Scopes.define(name).ok
    case function: Def    => enterFunction(Resolve.function(_)(function))
    case StmtDecl(stmt)   => Resolve.stmt(stmt)
    case stmt: Stmt       => Resolve.stmt(stmt)
    case klass: ClassDecl => Resolve.classDecl(klass)
  }

  def stmt: Stmt => Resolve = {
    case ExprStmt(expr)            => Resolve.expr(expr)
    case IfThen(condition, branch) => Resolve.expr(condition) >=> Resolve.stmt(branch)
    case IfThenElse(condition, thenBranch, elseBranch) =>
      Resolve.expr(condition) >=> Resolve.stmt(thenBranch) >=> Resolve.stmt(elseBranch)
    case returnStmt: ReturnStmt => Resolve.returnStmt(returnStmt)
    case While(condition, body) => Resolve.expr(condition) >=> Resolve.stmt(body)
    case blockStmt: BlockStmt   => Resolve.blockStmt(blockStmt)
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
            else fail(ResolutionError.declaredButNotDefined(name))
          }
          .getOrElse(Resolve.local(name)(expr))
      }
    case expr @ Assignment(name, value) => Resolve.expr(value) >=> Resolve.local(name)(expr)
    case Call(callee, arguments, _) =>
      Resolve.expr(callee) >=> { scopes =>
        arguments.foldLeft(scopes.ok)((acc, arg) => acc.flatMap(Resolve.expr(arg)))
      }
    case lambda: Lambda     => enterFunction(Resolve.lambda(_)(lambda))
    case Get(obj, _)        => Resolve.expr(obj)
    case Set(obj, _, value) => Resolve.expr(value) >=> Resolve.expr(obj)
  }

  /**
   * Resolves a function. Note: We are not declaring the name of the function in the scope because it is
   * assumed [[scanFunctions]] already did that.
   */
  def function(oldFunctionType: FunctionType): Def => Resolve = { case Def(name, params, body) =>
    Scopes.define(name).ok >=> Resolve.lambda(oldFunctionType)(Lambda(params, body))
  }

  def lambda(oldFunctionType: FunctionType): Lambda => Resolve = { case Lambda(params, body) =>
    val resolution = Scopes.start.ok >=> { contexts =>
      params.foldFailFast(contexts.ok) { (contexts, param) =>
        (Scopes.declare(param).ok >=> Scopes.define(param).ok)(contexts)
      }
    } >=> Resolve.blockStmt(BlockStmt(body)) >=> Scopes.end.ok

    resolution.andThen(_.map { case (scopes, locals, _) =>
      (scopes, locals, oldFunctionType) // exit function using the previous function type
    })
  }

  def returnStmt: ReturnStmt => Resolve = {
    // Note: Resolving a return statement involves checking if it's inside a function.
    def returnOrFail(keyword: Token, ifValid: => Resolve): Resolve = {
      case (_, _, FunctionType.None)               => Result.fail(ResolutionError.notInsideAFunction(keyword))
      case context @ (_, _, FunctionType.Function | FunctionType.Method) => ifValid(context)
    }

    {
      case ReturnStmt(keyword, Literal.None) => returnOrFail(keyword, _.ok)
      case ReturnStmt(keyword, value)        => returnOrFail(keyword, Resolve.expr(value))
    }
  }

  def classDecl: ClassDecl => Resolve = { case ClassDecl(name, methods) =>
    Scopes.declare(name).ok >=> Scopes.define(name).ok >=> { case (scopes, locals, functionType) =>
      methods.foldFailFast((scopes, locals, FunctionType.Method: FunctionType).ok) { (context, method) =>
        Resolve.function(functionType)(method)(context)
      }
    }
  }

  def exprWithDepth(depth: Int): Expr => Resolve = expr => { case (scopes, locals, functionType) =>
    (scopes, locals + (LocalExprKey(expr) -> depth), functionType).ok
  }

  def local(name: Token): Expr => Resolve = { expr =>
    { case context @ (scopes, _, _) =>
      val maybeFound = scopes.zipWithIndex.find { case (scope, _) =>
        scope.contains(name.lexeme)
      }
      maybeFound
        .map { case (_, i) => Resolve.exprWithDepth(i)(expr)(context) }
        .getOrElse(context.ok)
    }
  }

  def fail(failure: Failure): Resolve =
    _ => Result.fail(failure)

  private def enterFunction(toResolve: FunctionType => Resolve): Resolve = {
    case (scopes, locals, functionType) =>
      toResolve(functionType)(scopes, locals, FunctionType.Function)
  }
}
