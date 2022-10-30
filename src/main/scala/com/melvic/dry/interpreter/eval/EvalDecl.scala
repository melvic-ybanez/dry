package com.melvic.dry.interpreter.eval

import com.melvic.dry.ast.Decl.Let.{LetDecl, LetInit}
import com.melvic.dry.ast.Decl._
import com.melvic.dry.ast.{Decl, Stmt}
import com.melvic.dry.interpreter.values.Value.ToValue
import com.melvic.dry.interpreter.values.{Callable, DryClass, Value}
import com.melvic.dry.result.Result.implicits.ToResult

private[eval] trait EvalDecl extends EvalStmt {
  def decl: Evaluate[Decl] = {
    case stmt: StmtDecl => stmtDecl(stmt)
    case let: Let       => Evaluate.let(let)
    case function: Def  => Evaluate.defDecl(function)

    // Not a big fan of this. Declaration statements should be of type StmtDecl,
    // but sometimes I needed to create a statement directly in the statement parser (e.g. a syntactic-sugar),
    // and it didn't sound right to wrap them in StmtDecl, nor would it typecheck.
    // I can solve this issue by either moving the syntactic-sugar here in declaration parser
    // and wrap it in StmtDecl, or remove StmtDecl altogether.
    case stmt: Stmt => Evaluate.stmt(stmt)
  }

  def stmtDecl: Evaluate[Decl] = { case StmtDecl(stmt) =>
    Evaluate.stmt(stmt)
  }

  def let: Evaluate[Let] = {
    def letDecl: Evaluate[LetDecl] = { case LetDecl(name) =>
      _.define(name.lexeme, Value.None).unit.ok
    }

    def letInit: Evaluate[LetInit] = { case LetInit(name, init) =>
      env =>
        Evaluate
          .expr(init)(env)
          .flatMap { value =>
            env.define(name.lexeme, value).unit.ok
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
    _.defineWith(name.lexeme, Callable.Function(function, _)).unit.ok
  }

  def classDecl: Evaluate[ClassDecl] = { case ClassDecl(name, _) =>
    env => env.define(name.lexeme, Value.None).assign(name, DryClass(name.lexeme, env)).unit.ok
  }
}
