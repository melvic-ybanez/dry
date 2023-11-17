package com.melvic.dry.interpreter.eval

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Expr.Variable
import com.melvic.dry.ast.Stmt.CatchBlock.{CatchAll, CatchType, CatchTypedVar, CatchUntypedVar}
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
import com.melvic.dry.interpreter.values._
import com.melvic.dry.interpreter.{Env, ModuleManager, Run}
import com.melvic.dry.result.Failure.{Raised, RuntimeError}
import com.melvic.dry.result.Result.Result
import com.melvic.dry.result.Result.implicits._
import com.melvic.dry.result.{Failure, Result}

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

  def blockStmt(implicit context: Context[BlockStmt]): Out =
    blockStmtWith(Env.fromEnclosing)

  def blockStmtWith(envF: Env => Env)(implicit context: Context[BlockStmt]): Out = {
    val localEnv = envF(env)
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
      def handleException(raised: Raised) = {
        def invalidArg(got: String, paren: Token): Failure =
          RuntimeError.invalidArgument(Types.Exception, got, paren.line)

        lazy val raisedKind = DException.kindOf(raised.instance)

        def catchGeneric(paren: Token)(catchFromRaised: String => CatchBlock) =
          raisedKind.toRight(One(invalidArg("unknown exception", paren))).flatMap { raised =>
            evalCatchBlock(catchFromRaised(raised))
          }

        def evalCatchBlock: CatchBlock => Result[Option[Value]] = {
          case CatchType(exceptionKind, block, paren) =>
            Evaluate.variable(exceptionKind).flatMap {
              case DException(kind, _) if raisedKind.contains(kind.exceptionName) =>
                Evaluate.blockStmt(block).map(Some(_))
              case DException(_, _) => Right(None)
              case arg              => invalidArg(Value.typeOf(arg), paren).fail
            }
          case CatchTypedVar(Variable(instance), exceptionKind, block, paren) =>
            Evaluate.variable(exceptionKind).flatMap {
              case DException(kind, _) if raisedKind.contains(kind.exceptionName) =>
                Evaluate
                  .blockStmtWith { env =>
                    val localEnv = Env.fromEnclosing(env)
                    localEnv.define(instance, raised.instance)
                  }(block)
                  .map(Some(_))
              case DException(_, _) => Right(None)
              case arg              => invalidArg(Value.typeOf(arg), paren).fail
            }
          case CatchAll(block, paren) =>
            catchGeneric(paren)(raised =>
              CatchType(Variable(Token(TokenType.Identifier, raised, paren.line)), block, paren)
            )
          case CatchUntypedVar(instance, block, paren) =>
            catchGeneric(paren)(raised =>
              CatchTypedVar(instance, Variable(Token(TokenType.Identifier, raised, paren.line)), block, paren)
            )
        }

        def findCatchBlock(catchBlocks: Nel[CatchBlock]): Out =
          catchBlocks match {
            case One(head) => evalCatchBlock(head).flatMap(_.fold[Out](raised.fail)(_.ok))
            case Many(head, tail) =>
              evalCatchBlock(head).flatMap(_.fold[Out](findCatchBlock(tail))(_.ok))
          }

        findCatchBlock(catchBlocks)
      }
      Evaluate
        .blockStmt(tryBlock)
        .fold(
          {
            case One(failure @ Failure.Raised(_)) => handleException(failure)
            case One(RuntimeError(kind, token, message)) =>
              DException(kind, env).call(token)(Value.Str(message) :: Nil).flatMap { instance =>
                handleException(Raised(instance.asInstanceOf[DInstance]))
              }
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
