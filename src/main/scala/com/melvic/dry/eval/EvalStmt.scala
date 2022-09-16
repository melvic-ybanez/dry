package com.melvic.dry.eval

import com.melvic.dry.Env.LocalEnv
import com.melvic.dry.Value.{Unit => VUnit}
import com.melvic.dry.ast.Stmt.IfStmt.{IfThen, IfThenElse}
import com.melvic.dry.ast.Stmt.Loop.While
import com.melvic.dry.ast.Stmt.{BlockStmt, ExprStmt, IfStmt}
import com.melvic.dry.ast.{Decl, Stmt}
import com.melvic.dry.eval.implicits._
import com.melvic.dry.result.Result
import com.melvic.dry.{Env, Value}

private[eval] trait EvalStmt {
  def stmt: Evaluate[Stmt] = {
    case expr: ExprStmt   => Evaluate.exprStmt(expr)
    case block: BlockStmt => Evaluate.blockStmt(block)
    case ifStmt: IfStmt   => Evaluate.ifStmt(ifStmt)
    case whileStmt: While => Evaluate.whileStmt(whileStmt)
  }

  def exprStmt: Evaluate[ExprStmt] = { case ExprStmt(expr) =>
    Evaluate.expr(expr).mapValue(Value.ExprStmt)
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
    Evaluate.expr(ifStmt.condition).andThen { condResult =>
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

  def whileStmt: Evaluate[While] = { case While(condition, body) =>
    def recurse(env: Env): EvalOut =
      Evaluate.expr(condition)(env).flatMap { case (condition, env) =>
        if (!isTruthy(condition)) Result.succeed(VUnit, env)
        else
          Evaluate.stmt(body)(env).flatMap { case (_, env) =>
            recurse(env)
          }
      }

    recurse
  }
}
