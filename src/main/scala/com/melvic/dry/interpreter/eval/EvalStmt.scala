package com.melvic.dry.interpreter.eval

import com.melvic.dry.Token
import com.melvic.dry.ast.Stmt.IfStmt.{IfThen, IfThenElse}
import com.melvic.dry.ast.Stmt.Loop.While
import com.melvic.dry.ast.Stmt._
import com.melvic.dry.ast.{Decl, Stmt}
import com.melvic.dry.interpreter.Value.{Returned, Unit => VUnit}
import com.melvic.dry.interpreter.eval.Context.implicits._
import com.melvic.dry.interpreter.eval.Evaluate.Out
import com.melvic.dry.interpreter.values.Value.ToValue
import com.melvic.dry.interpreter.values.{DModule, Value}
import com.melvic.dry.interpreter.{Env, ModuleManager, Run}
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.implicits._

//noinspection ScalaWeakerAccess
private[eval] trait EvalStmt {
  def stmt(implicit context: Context[Stmt]): Out = node match {
    case expr: ExprStmt         => Evaluate.exprStmt(expr)
    case block: BlockStmt       => Evaluate.blockStmt(block)
    case ifStmt: IfStmt         => Evaluate.ifStmt(ifStmt)
    case whileStmt: While       => Evaluate.whileStmt(whileStmt)
    case deleteStmt: DeleteStmt => Evaluate.deleteStmt(deleteStmt)
    case returnStmt: ReturnStmt => Evaluate.returnStmt(returnStmt)
    case importStmt: Import     => Evaluate.importStmt(importStmt)
  }

  def exprStmt(implicit context: Context[ExprStmt]): Out =
    Evaluate.expr(node.expr).map(_.unit)

  def blockStmt(implicit context: Context[BlockStmt]): Out = {
    val localEnv = Env.fromEnclosing(env)
    def recurse(outcome: Out, decls: List[Decl]): Out = {
      decls match {
        case Nil => outcome
        case decl :: rest =>
          outcome.flatMap {
            case returned: Returned => returned.ok
            case _ => recurse(Evaluate.decl(Context(decl, localEnv, locals, sourcePaths)), rest)
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
    def recurse(out: Value): Out =
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

  def deleteStmt(implicit context: Context[DeleteStmt]): Out = node match {
    case DeleteStmt(obj, key) =>
      Evaluate.index(obj, key) { dict =>
        Result.fromOption(dict.deleteByKey(key), RuntimeError.undefinedKey(key))
      }
  }

  def importStmt(implicit context: Context[Import]): Out = {
    val moduleComponents = node.path.map(Token.show)

    Result
      .fromOption(
        ModuleManager.fullPathOf(moduleComponents, sourcePaths),
        RuntimeError.moduleNotFound(moduleComponents.mkString, node.name)
      )
      .flatMap(path =>
        Run
          .path(path.toString, sourcePaths)
          .map(moduleEnv => env.define(node.name.lexeme, DModule(moduleEnv)).unit)
      )

  }
}
