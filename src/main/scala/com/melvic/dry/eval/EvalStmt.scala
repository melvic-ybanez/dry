package com.melvic.dry.eval

import com.melvic.dry.Env.LocalEnv
import com.melvic.dry.Value.{Unit => VUnit}
import com.melvic.dry.ast.Stmt.IfStmt.{IfThen, IfThenElse}
import com.melvic.dry.ast.Stmt.{BlockStmt, ExprStmt, IfStmt, PrintStmt}
import com.melvic.dry.ast.{Decl, Stmt}
import com.melvic.dry.eval.implicits._
import com.melvic.dry.implicits._
import com.melvic.dry.result.Result
import com.melvic.dry.{Env, Value}

private[eval] trait EvalStmt {
  def stmt: Evaluate[Stmt] = {
    case expr: ExprStmt   => Evaluate.exprStmt(expr)
    case print: PrintStmt => Evaluate.printStmt(print)
    case block: BlockStmt => Evaluate.blockStmt(block)
    case ifStmt: IfStmt   => Evaluate.ifStmt(ifStmt)
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

  def ifStmt: Evaluate[IfStmt] = { ifStmt =>
    Evaluate.expr(ifStmt.condition).map { condResult =>
      ifStmt match {
        case IfThen(_, thenBranch) =>
          condResult.flatMap { case (value, env) =>
            if (isTruthy(value)) Evaluate.stmt(thenBranch)(env)
            else Result.succeed(VUnit, env)
          }
        case IfThenElse(_, thenBranch, elseBranch) =>
          condResult.flatMap { case (value, env) =>
            if (isTruthy(value)) Evaluate.stmt(thenBranch)(env)
            else Evaluate.stmt(elseBranch)(env)
          }
      }
    }
  }
}
