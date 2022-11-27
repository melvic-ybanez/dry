package com.melvic.dry.interpreter.eval

import com.melvic.dry.Token
import com.melvic.dry.ast.Stmt.IfStmt.{IfThen, IfThenElse}
import com.melvic.dry.ast.Stmt.Loop.While
import com.melvic.dry.ast.Stmt._
import com.melvic.dry.ast.{Decl, Stmt}
import com.melvic.dry.interpreter.Value.{Returned, Unit => VUnit}
import com.melvic.dry.interpreter.eval.implicits._
import com.melvic.dry.interpreter.values.Value.{Str, ToValue}
import com.melvic.dry.interpreter.values.{DModule, Value}
import com.melvic.dry.interpreter.{Env, Keys, ModuleLoader, Run}
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.implicits._

import java.nio.file.Paths

private[eval] trait EvalStmt {
  def stmt: Evaluate[Stmt] = {
    case expr: ExprStmt         => Evaluate.exprStmt(expr)
    case block: BlockStmt       => Evaluate.blockStmt(block)
    case ifStmt: IfStmt         => Evaluate.ifStmt(ifStmt)
    case whileStmt: While       => Evaluate.whileStmt(whileStmt)
    case returnStmt: ReturnStmt => Evaluate.returnStmt(returnStmt)
    case importStmt: Import     => Evaluate.importStmt(importStmt)
  }

  def exprStmt: Evaluate[ExprStmt] = { case ExprStmt(expr) =>
    Evaluate.expr(expr).map(_.unit)
  }

  def blockStmt: Evaluate[BlockStmt] = { case BlockStmt(decls) =>
    env =>
      val localEnv = Env.fromEnclosing(env)
      def recurse(outcome: EvalOut, decls: List[Decl]): EvalOut = {
        decls match {
          case Nil => outcome
          case decl :: rest =>
            outcome.flatMap {
              case returned: Returned => returned.ok
              case _                  => recurse(Evaluate.decl(decl)(localEnv), rest)
            }
        }
      }

      recurse(Result.succeed(VUnit), decls)
  }

  def ifStmt: Evaluate[IfStmt] = { ifStmt => env =>
    Evaluate
      .expr(ifStmt.condition)
      .andThen { condResult =>
        ifStmt match {
          case IfThen(_, thenBranch) =>
            condResult.flatMap { value =>
              if (isTruthy(value)) Evaluate.stmt(thenBranch)(env)
              else Result.succeed(VUnit)
            }
          case IfThenElse(_, thenBranch, elseBranch) =>
            condResult.flatMap { value =>
              if (isTruthy(value)) Evaluate.stmt(thenBranch)(env)
              else Evaluate.stmt(elseBranch)(env)
            }
        }
      }(env)
  }

  def whileStmt: Evaluate[While] = { case While(condition, body) =>
    env =>
      def recurse(out: Value): EvalOut =
        Evaluate.expr(condition)(env).flatMap { condition =>
          if (!isTruthy(condition)) Result.succeed(out)
          else
            Evaluate.stmt(body)(env).flatMap {
              case returned: Returned => Result.succeed(returned)
              case _                  => recurse(VUnit)
            }
        }

      recurse(VUnit)
  }

  def returnStmt: Evaluate[ReturnStmt] = { case ReturnStmt(_, value) =>
    Evaluate.expr(value).map(Returned)
  }

  def importStmt: Evaluate[Import] = { case stmt @ Import(path) =>
    env =>
      // Since the interpreter is exposing the main module's path to the user,
      // we can summon it here, though it assumes that it's located in the layer below
      // the natives (hence, `height - 2`)
      val mainModule = env.at(env.height - 2, Keys.MainModule).map {
        case Str(value) => value
        case _          => "."
      } getOrElse "."

      Run
        .path(
          mainModule,
          ModuleLoader(Paths.get(mainModule)).fullPathOf(path.map(Token.show).mkString(".")).toString
        )
        .map(moduleEnv => env.define(stmt.name.lexeme, DModule(moduleEnv)).unit)
  }
}
