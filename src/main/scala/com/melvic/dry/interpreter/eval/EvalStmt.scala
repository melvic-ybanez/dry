package com.melvic.dry.interpreter.eval

import com.melvic.dry.Token
import com.melvic.dry.ast.Stmt.IfStmt.{IfThen, IfThenElse}
import com.melvic.dry.ast.Stmt.Loop.While
import com.melvic.dry.ast.Stmt._
import com.melvic.dry.ast.{Decl, Stmt}
import com.melvic.dry.interpreter.Value.{Returned, Unit => VUnit}
import com.melvic.dry.interpreter.eval.Context.implicits._
import com.melvic.dry.interpreter.eval.Evaluate.Out
import com.melvic.dry.interpreter.values.Value.{Str, ToValue}
import com.melvic.dry.interpreter.values.{DModule, Value}
import com.melvic.dry.interpreter.{Env, Keys, ModuleLoader, Run}
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.implicits._

import java.nio.file.Paths

private[eval] trait EvalStmt {
  def stmt(implicit context: Context[Stmt]): Out = node match {
    case expr: ExprStmt         => Evaluate.exprStmt(expr)
    case block: BlockStmt       => Evaluate.blockStmt(block)
    case ifStmt: IfStmt         => Evaluate.ifStmt(ifStmt)
    case whileStmt: While       => Evaluate.whileStmt(whileStmt)
    case returnStmt: ReturnStmt => Evaluate.returnStmt(returnStmt)
    case importStmt: Import     => Evaluate.importStmt(importStmt)
  }

  def exprStmt(implicit context: Context[ExprStmt]): Out =
    Evaluate.expr(node.expr).map(_.unit)

  def blockStmt(implicit context: Context[BlockStmt]): Out = {
    val localEnv = Env.fromEnclosing(env)
    def recurse(outcome: EvalOut, decls: List[Decl]): EvalOut = {
      decls match {
        case Nil => outcome
        case decl :: rest =>
          outcome.flatMap {
            case returned: Returned => returned.ok
            case _                  => recurse(Evaluate.decl(Context(decl, localEnv)), rest)
          }
      }
    }

    recurse(Result.succeed(VUnit), node.declarations)
  }

  def ifStmt(implicit context: Context[IfStmt]): Out =
    Evaluate
      .expr(node.condition)
      .flatMap { value =>
        node match {
          case IfThen(_, thenBranch) =>
            if (isTruthy(value)) Evaluate.stmt(thenBranch)
            else Result.succeed(VUnit)
          case IfThenElse(_, thenBranch, elseBranch) =>
            if (isTruthy(value)) Evaluate.stmt(thenBranch)
            else Evaluate.stmt(elseBranch)
        }
      }

  def whileStmt(implicit context: Context[While]): Out = {
    def recurse(out: Value): EvalOut =
      Evaluate.expr(node.condition).flatMap { condition =>
        if (!isTruthy(condition)) Result.succeed(out)
        else
          Evaluate.stmt(node.body).flatMap {
            case returned: Returned => Result.succeed(returned)
            case _                  => recurse(VUnit)
          }
      }

    recurse(VUnit)
  }

  def returnStmt(implicit context: Context[ReturnStmt]): Out =
    Evaluate.expr(node.value).map(Returned)

  def importStmt(implicit context: Context[Import]): Out = {
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
        ModuleLoader(Paths.get(mainModule)).fullPathOf(node.path.map(Token.show).mkString(".")).toString
      )
      .map(moduleEnv => env.define(node.name.lexeme, DModule(moduleEnv)).unit)
  }
}
