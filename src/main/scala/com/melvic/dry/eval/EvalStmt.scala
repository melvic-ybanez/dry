package com.melvic.dry.eval

import com.melvic.dry.Env.LocalEnv
import com.melvic.dry.Value.{Unit => VUnit}
import com.melvic.dry.ast.Stmt.{BlockStmt, ExprStmt, PrintStmt}
import com.melvic.dry.ast.{Decl, Stmt}
import com.melvic.dry.eval.implicits._
import com.melvic.dry.result.Result
import com.melvic.dry.{Env, Value}

private[eval] trait EvalStmt {
  def stmt: Evaluate[Stmt] = {
    case expr: ExprStmt   => exprStmt(expr)
    case print: PrintStmt => printStmt(print)
    case block: BlockStmt => blockStmt(block)
  }

  def exprStmt: Evaluate[ExprStmt] = { case ExprStmt(expr) =>
    Evaluate.expr(expr).mapValue(Value.ExprStmt)
  }

  def printStmt: Evaluate[PrintStmt] = { case PrintStmt(expr) =>
    Evaluate.expr(expr).mapValue { value =>
      println(Value.show(value))
      VUnit
    }
  }

  def blockStmt: Evaluate[BlockStmt] = { case BlockStmt(decls) =>
    env =>
      def recurse(outcome: EvalOut, decls: List[Decl]): EvalOut =
        decls match {
          case Nil => outcome
          case decl :: rest =>
            outcome.flatMap { case (_, env) =>
              recurse(Evaluate.decl(decl)(env), rest)
            }
        }

      recurse(Result.succeed(VUnit, Env.localEnv(env)), decls).map {
        case (value, LocalEnv(_, enclosing)) => (value, enclosing)
        case (value, env)                    => (value, env)
      }
  }
}
