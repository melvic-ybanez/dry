package com.melvic.dry.eval

import com.melvic.dry.ast.Decl
import com.melvic.dry.ast.Decl.StmtDecl
import com.melvic.dry.eval.Evaluate.Evaluate

private[eval] trait EvalDecl extends EvalStmt {
  def decl: Evaluate[Decl] = {
    case stmt: StmtDecl => stmtDecl(stmt)
  }

  def stmtDecl: Evaluate[Decl] = {
    case StmtDecl(stmt) => Evaluate.stmt(stmt)
  }
}
