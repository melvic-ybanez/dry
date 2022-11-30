package com.melvic.dry.interpreter.eval

import com.melvic.dry.ast.Decl.Let.{LetDecl, LetInit}
import com.melvic.dry.ast.Decl._
import com.melvic.dry.ast.{Decl, Stmt}
import com.melvic.dry.interpreter.eval.Evaluate.Out
import com.melvic.dry.interpreter.values.Value.ToValue
import com.melvic.dry.interpreter.values.{Callable, DClass, Value}
import com.melvic.dry.lexer.Lexemes
import com.melvic.dry.result.Result.implicits.ToResult
import Context.implicits._

private[eval] trait EvalDecl extends EvalStmt {
  def decl(implicit context: Context[Decl]): Out = node match {
    case stmt: StmtDecl   => stmtDecl(stmt)(env)
    case let: Let         => Evaluate.let(let)(env)
    case function: Def    => Evaluate.defDecl(function)(env)
    case klass: ClassDecl => Evaluate.classDecl(klass)(env)

    // Not a big fan of this. Declaration statements should be of type StmtDecl,
    // but sometimes I needed to create a statement directly in the statement parser (e.g. a syntactic-sugar),
    // and it didn't sound right to wrap them in StmtDecl, nor would it typecheck.
    // I can solve this issue by either moving the syntactic-sugar here in declaration parser
    // and wrap it in StmtDecl, or remove StmtDecl altogether.
    case stmt: Stmt => Evaluate.stmt(stmt)(env)
  }

  def stmtDecl: Evaluate[Decl] = { case StmtDecl(stmt) =>
    Evaluate.stmt(stmt)
  }

  def let: Evaluate[Let] = {
    def letDecl: Evaluate[LetDecl] = { case LetDecl(name) =>
      _.define(name, Value.None).unit.ok
    }

    def letInit: Evaluate[LetInit] = { case LetInit(name, init) =>
      env =>
        Evaluate
          .expr(init)(env)
          .flatMap { value =>
            env.define(name, value).unit.ok
          }
    }

    {
      case let: LetDecl => letDecl(let)
      case let: LetInit => letInit(let)
    }
  }

  /**
   * Defines a named function. Note: We may no longer need this if we decide to treat `def` functions as
   * syntactic sugars for lambda-expressions stored to variables.
   */
  def defDecl: Evaluate[Def] = { case function @ Def(name, _, _) =>
    _.defineWith(name.lexeme, Callable.Function(function, _, isInit = false)).unit.ok
  }

  def classDecl: Evaluate[ClassDecl] = { case ClassDecl(name, methods) =>
    env =>
      env.define(name, Value.None)
      val klass = DClass(
        name.lexeme,
        methods
          .map(method =>
            method.name.lexeme -> Callable.Function(method, env, method.name.lexeme == Lexemes.Init)
          )
          .toMap,
        env
      )
      env.assign(name, klass).unit.ok
  }
}
