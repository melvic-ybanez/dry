package com.melvic.dry.interpreter.eval

import com.melvic.dry.ast.Decl.Let.{LetDecl, LetInit}
import com.melvic.dry.ast.Decl._
import com.melvic.dry.ast.{Decl, Stmt}
import com.melvic.dry.interpreter.eval.Context.implicits._
import com.melvic.dry.interpreter.eval.Evaluate.Out
import com.melvic.dry.interpreter.values.Value.ToValue
import com.melvic.dry.interpreter.values.{Callable, DClass, Value}
import com.melvic.dry.lexer.Lexemes
import com.melvic.dry.result.Result.implicits.ToResult

private[eval] trait EvalDecl extends EvalStmt {
  def decl(implicit context: Context[Decl]): Out = node match {
    case stmt: StmtDecl   => Evaluate.stmtDecl(stmt)
    case let: Let         => Evaluate.let(let)
    case function: Def    => Evaluate.defDecl(function)
    case klass: ClassDecl => Evaluate.classDecl(klass)

    // Not a big fan of this. Declaration statements should be of type StmtDecl,
    // but sometimes I needed to create a statement directly in the statement parser (e.g. a syntactic-sugar),
    // and it didn't sound right to wrap them in StmtDecl, nor would it typecheck.
    // I can solve this issue by either moving the syntactic-sugar here in declaration parser
    // and wrap it in StmtDecl, or remove StmtDecl altogether.
    case stmt: Stmt => Evaluate.stmt(stmt)
  }

  def stmtDecl(implicit context: Context[StmtDecl]): Out =
    Evaluate.stmt(node.stmt)

  def let(implicit context: Context[Let]): Out = {
    def letDecl(implicit context: Context[LetDecl]): Out =
      env.define(node.name, Value.None).unit.ok

    def letInit(implicit context: Context[LetInit]): Out =
      Evaluate
        .expr(node.init)
        .flatMap { value =>
          env.define(node.name, value).unit.ok
        }

    node match {
      case let: LetDecl => letDecl(let)
      case let: LetInit => letInit(let)
    }
  }

  /**
   * Defines a named function. Note: We may no longer need this if we decide to treat `def` functions as
   * syntactic sugars for lambda-expressions stored to variables.
   */
  def defDecl(implicit context: Context[Def]): Out =
    env.defineWith(node.name.lexeme, Callable.Function(node, _, isInit = false)).unit.ok

  def classDecl(implicit context: Context[ClassDecl]): Out =
    node match {
      case ClassDecl(name, methods) =>
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
