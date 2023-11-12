package com.melvic.dry.interpreter.eval

import com.melvic.dry.Token
import com.melvic.dry.ast.Expr.Variable
import com.melvic.dry.ast.Stmt.IfStmt.{IfThen, IfThenElse}
import com.melvic.dry.ast.Stmt.Loop.While
import com.melvic.dry.ast.Stmt._
import com.melvic.dry.ast.{Decl, Stmt}
import com.melvic.dry.aux.Nel
import com.melvic.dry.aux.Nel.{Many, One}
import com.melvic.dry.interpreter.Value.{Returned, Unit => VUnit}
import com.melvic.dry.interpreter.eval.Context.implicits._
import com.melvic.dry.interpreter.eval.Evaluate.Out
import com.melvic.dry.interpreter.values.Value.{ToValue, Types}
import com.melvic.dry.interpreter.values.{DDictionary, DException, DList, DModule, Value}
import com.melvic.dry.interpreter.{Env, ModuleManager, Run}
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.Result
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
    case tryCatch: TryCatch     => Evaluate.tryCatch(tryCatch)
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
    case DeleteStmt(obj, key, token) =>
      Evaluate.index(obj, key, token) {
        case (dict: DDictionary, evaluatedKey) =>
          Result.fromOption(dict.deleteByKey(evaluatedKey), RuntimeError.undefinedKey(key, token))
        case (list: DList, evaluatedKey) =>
          Evaluate.accessNumericIndex(evaluatedKey, key, list.size, token)(list.deleteByIndex(_).ok)
      }
  }

  def tryCatch(implicit context: Context[TryCatch]): Out = node match {
    case TryCatch(tryBlock, catchBlocks) =>
      Evaluate
        .blockStmt(tryBlock)
        .fold(
          {
            case One(runtimeError @ RuntimeError(kind, _, _)) =>
              def invalidArg(got: String, paren: Token): Result[Option[Value]] =
                RuntimeError.invalidArgument(Types.Exception, got, paren.line).fail

              def evalCatchBlock: CatchBlock => Result[Option[Value]] = {
                case CatchBlock(exception: Variable, block, paren) =>
                  Evaluate.variable(exception).flatMap {
                    case DException(`kind`, _) => Evaluate.blockStmt(block).map(Some(_))
                    case DException(_, _)      => Right(None)
                    case arg                   => invalidArg(Value.typeOf(arg), paren)
                  }
                case CatchBlock(_, _, paren) => invalidArg("expression", paren)
              }

              def findCatchBlock(catchBlocks: Nel[CatchBlock]): Out =
                catchBlocks match {
                  case One(head) => evalCatchBlock(head).flatMap(_.fold[Out](runtimeError.fail)(_.ok))
                  case Many(head, tail) =>
                    evalCatchBlock(head).flatMap(_.fold[Out](findCatchBlock(tail))(_.ok))
                }

              findCatchBlock(catchBlocks)
            case errors => Result.failAll(errors)
          },
          _.ok
        )
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
