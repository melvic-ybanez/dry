package com.melvic.dry.resolver

import com.melvic.dry.Token
import com.melvic.dry.ast.Decl.Let.{LetDecl, LetInit}
import com.melvic.dry.ast.Decl.{ClassDecl, Def, StmtDecl}
import com.melvic.dry.ast.Expr._
import com.melvic.dry.ast.Stmt.IfStmt.{IfThen, IfThenElse}
import com.melvic.dry.ast.Stmt.Loop.While
import com.melvic.dry.ast.Stmt.{BlockStmt, ExprStmt, Import, ReturnStmt}
import com.melvic.dry.ast.{Decl, Expr, Stmt}
import com.melvic.dry.aux.HasFlatMap._
import com.melvic.dry.aux.implicits._
import com.melvic.dry.lexer.Lexemes
import com.melvic.dry.resolver.ScopesFunction._
import com.melvic.dry.result.Failure.ResolverError
import com.melvic.dry.result.Result.ResultFrom
import com.melvic.dry.result.Result.implicits.ToResult
import com.melvic.dry.result.{Failure, Result}

//noinspection SpellCheckingInspection
object Resolve {
  type Resolve = ResultFrom[Context]

  def resolveAll: List[Decl] => Resolve = decls =>
    Resolve.scanFunctions(decls).andThen(_.flatMap(Resolve.decls(decls)))

  def blockStmt: BlockStmt => Resolve = block =>
    Scopes.start.ok >=> Resolve.resolveAll(block.declarations) >=> Scopes.end.ok

  def decls: List[Decl] => Resolve = decls =>
    context => decls.foldFailFast(context.ok)((context, decl) => Resolve.decl(decl)(context))

  private def scanFunctions: List[Decl] => Resolve = decls =>
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
    case function: Def  => enterFunction(Resolve.function(_)(function))
    case StmtDecl(stmt) => Resolve.stmt(stmt)
    case stmt: Stmt     => Resolve.stmt(stmt)
    case klass: ClassDecl =>
      context => Resolve.classDecl(context.classType)(klass)(context.withClassType(ClassType.Class))
  }

  def stmt: Stmt => Resolve = {
    case ExprStmt(expr)            => Resolve.expr(expr)
    case IfThen(condition, branch) => Resolve.expr(condition) >=> Resolve.stmt(branch)
    case IfThenElse(condition, thenBranch, elseBranch) =>
      Resolve.expr(condition) >=> Resolve.stmt(thenBranch) >=> Resolve.stmt(elseBranch)
    case returnStmt: ReturnStmt => Resolve.returnStmt(returnStmt)
    case While(condition, body) => Resolve.expr(condition) >=> Resolve.stmt(body)
    case blockStmt: BlockStmt   => Resolve.blockStmt(blockStmt)
    case importStmt: Import     => Scopes.declare(importStmt.name).ok >=> Scopes.define(importStmt.name).ok
  }

  def expr: Expr => Resolve = {
    case _: Literal                     => _.ok
    case Unary(_, operand)              => Resolve.expr(operand)
    case Binary(left, _, right)         => Resolve.expr(left) >=> Resolve.expr(right)
    case Logical(left, _, right)        => Resolve.expr(left) >=> Resolve.expr(right)
    case Grouping(expr)                 => Resolve.expr(expr)
    case variable: Variable             => Resolve.variable(variable)
    case expr @ Assignment(name, value) => Resolve.expr(value) >=> Resolve.local(name)(expr)
    case Call(callee, arguments, _) =>
      Resolve.expr(callee) >=> { scopes =>
        arguments.foldLeft(scopes.ok)((acc, arg) => acc.flatMap(Resolve.expr(arg)))
      }
    case lambda: Lambda     => enterFunction(Resolve.lambda(_)(lambda))
    case Get(obj, _)        => Resolve.expr(obj)
    case Set(obj, _, value) => Resolve.expr(value) >=> Resolve.expr(obj)
    case self: Self         => Resolve.self(self)
    case dict: Dictionary   => Resolve.dictionary(dict)
  }

  def variable: Variable => Resolve = { case expr @ Variable(name) =>
    Scopes.resolveFromHead { scope =>
      scope
        .get(name.lexeme)
        .map { found =>
          if (found) Resolve.local(name)(expr)
          else fail(ResolverError.declaredButNotDefined(name))
        }
        .getOrElse(Resolve.local(name)(expr))
    }
  }

  /**
   * Resolves a function. Note: We are not declaring the name of the function in the scope because it is
   * assumed [[scanFunctions]] already did that.
   */
  def function(oldFunctionType: FunctionType): Def => Resolve = { case Def(name, params, body) =>
    Scopes.define(name).ok >=> Resolve.lambda(oldFunctionType)(Lambda(params, body))
  }

  def lambda(oldFunctionType: FunctionType): Lambda => Resolve = { case Lambda(params, body) =>
    val result = Scopes.start.ok >=> { contexts =>
      params.foldFailFast(contexts.ok) { (contexts, param) =>
        (Scopes.declare(param).ok >=> Scopes.define(param).ok)(contexts)
      }
    } >=> Resolve.blockStmt(BlockStmt(body)) >=> Scopes.end.ok

    result.andThen(_.map(_.withFunctionType(oldFunctionType)))
  }

  def returnStmt: ReturnStmt => Resolve = {
    // Note: Resolving a return statement involves checking if it's inside a function.
    def returnOrFail(keyword: Token)(ifValid: => Resolve): Resolve = {
      case Context(_, _, FunctionType.None, _) => ResolverError.notInsideAFunction(keyword).fail
      case context @ Context(_, _, FunctionType.Function | FunctionType.Method | FunctionType.Init, _) =>
        ifValid(context)
    }

    {
      case ReturnStmt(keyword, Literal.None) => returnOrFail(keyword)(_.ok)
      case ReturnStmt(keyword, value) =>
        returnOrFail(keyword) {
          case Context(_, _, FunctionType.Init, _) => ResolverError.returnFromInit(keyword).fail
          case context                             => Resolve.expr(value)(context)
        }
    }
  }

  def classDecl(oldClassType: ClassType): ClassDecl => Resolve = { case ClassDecl(name, methods) =>
    def resolveMethods: Resolve = context =>
      methods.foldFailFast(context.withFunctionType(FunctionType.Method).ok) { case (context, method) =>
        val functionType = if (method.name.lexeme == Lexemes.Init) FunctionType.Init else FunctionType.Method
        Resolve.function(context.functionType)(method)(context.withFunctionType(functionType))
      }

    val result = Scopes.declare(name).ok >=> Scopes.define(name).ok >=>
      Scopes.start.ok >=> Scopes.put(Lexemes.Self).ok >=> resolveMethods >=> Scopes.end.ok
    result.andThen(_.map(_.copy(classType = oldClassType)))
  }

  def self: Self => Resolve = {
    case expr @ Self(keyword) => {
      case Context(_, _, _, ClassType.None) => ResolverError.notInsideAClass(keyword).fail
      case context                          => Resolve.local(keyword)(expr)(context)
    }
  }

  def dictionary: Dictionary => Resolve = { case Dictionary(table) =>
    context =>
      table.toList.foldFailFast(context.ok) { case (context, (_, value)) =>
        Resolve.expr(value)(context)
      }
  }

  private def exprWithDepth(depth: Int): Expr => Resolve = expr =>
    context => context.copy(locals = context.locals + (LocalExprKey(expr) -> depth)).ok

  def local(name: Token): Expr => Resolve = { expr => context =>
    val maybeFound = context.scopes.zipWithIndex.find { case (scope, _) =>
      scope.contains(name.lexeme)
    }
    maybeFound
      .map { case (_, i) => Resolve.exprWithDepth(i)(expr)(context) }
      .getOrElse(context.ok)
  }

  def fail(failure: Failure): Resolve =
    _ => Result.fail(failure)

  private def enterFunction(toResolve: FunctionType => Resolve): Resolve = context =>
    toResolve(context.functionType)(context.withFunctionType(FunctionType.Function))
}
