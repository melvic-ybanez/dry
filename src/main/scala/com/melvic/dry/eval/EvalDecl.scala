package com.melvic.dry.eval

import com.melvic.dry.Value
import com.melvic.dry.ast.Decl
import com.melvic.dry.ast.Decl.{Let, LetDecl, LetInit, StmtDecl}
import com.melvic.dry.eval.Evaluate.Evaluate
import com.melvic.dry.implicits._
import com.melvic.dry.result.Result.implicits.ToResult

private[eval] trait EvalDecl extends EvalStmt {
  def decl: Evaluate[Decl] = {
    case stmt: StmtDecl => stmtDecl(stmt)
    case let: Let       => Evaluate.let(let)
  }

  def stmtDecl: Evaluate[Decl] = { case StmtDecl(stmt) =>
    Evaluate.stmt(stmt)
  }

  def let: Evaluate[Let] = {
    def letDecl: Evaluate[LetDecl] = { case LetDecl(name) =>
      env => (Value.Unit, env.register(name.lexeme, Value.None)).ok
    }

    def letInit: Evaluate[LetInit] = { case LetInit(name, init) =>
      Evaluate
        .expr(init)
        .map(
          _.flatMap { case (value, env) =>
            (Value.Unit, env.register(name.lexeme, value)).ok
          }
        )
    }

    {
      case let: LetDecl => letDecl(let)
      case let: LetInit => letInit(let)
    }
  }
}
