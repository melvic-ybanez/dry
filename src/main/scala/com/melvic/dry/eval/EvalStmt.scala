package com.melvic.dry.eval

import com.melvic.dry.Value
import com.melvic.dry.Value.{Unit => VUnit}
import com.melvic.dry.ast.Stmt
import com.melvic.dry.ast.Stmt.{ExprStmt, PrintStmt}
import com.melvic.dry.eval.Evaluate.Evaluate

private[eval] trait EvalStmt {
  def stmt: Evaluate[Stmt] = {
    case stmt: ExprStmt  => exprStmt(stmt)
    case stmt: PrintStmt => printStmt(stmt)
  }

  def exprStmt: Evaluate[ExprStmt] = { case ExprStmt(expr) =>
    Evaluate.expr(expr).map(_ => VUnit)
  }

  def printStmt: Evaluate[PrintStmt] = { case PrintStmt(expr) =>
    Evaluate.expr(expr).map { value =>
      println(Value.show(value))
      VUnit
    }
  }
}
