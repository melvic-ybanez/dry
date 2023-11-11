package com.melvic.dry.interpreter.values

import com.melvic.dry.Token
import com.melvic.dry.ast.Decl.Def
import com.melvic.dry.ast.Stmt.BlockStmt
import com.melvic.dry.ast.{Decl, Expr}
import com.melvic.dry.interpreter.Env
import com.melvic.dry.interpreter.eval.{Context, Evaluate}
import com.melvic.dry.interpreter.values.Callable.Call
import com.melvic.dry.interpreter.values.Value.Returned
import com.melvic.dry.lexer.Lexemes
import com.melvic.dry.resolver.Locals
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.Result
import com.melvic.dry.result.Result.implicits.ToResult

import java.nio.file.Path

private[interpreter] trait Callable extends Value {
  def arity: Int

  def enclosing: Env

  def call(token: Token): Call

  protected def local: Env = Env.fromEnclosing(enclosing)
}

object Callable {
  type Call = PartialFunction[List[Value], Result[Value]]

  abstract class FunctionLike(
      val params: List[Token],
      val body: List[Decl],
      locals: Locals,
      sourcePaths: List[Path]
  ) extends Callable {
    def isInit: Boolean

    override def arity = params.size

    override def call(token: Token): Call = { args =>
      val env = params.zip(args).foldLeft(local) { case (env, (param, arg)) =>
        env.define(param.lexeme, arg)
      }
      Evaluate
        .blockStmt(Context(BlockStmt(body), env, locals, sourcePaths))
        .map(value => if (isInit) init.getOrElse(value) else value)
        .map {
          case Returned(value) => if (isInit) init.getOrElse(Value.None) else value
          case value           => value
        }
    }

    lazy val init: Option[Value] = enclosing.at(0, Lexemes.Init)
  }

  final case class Function(
      function: Def,
      enclosing: Env,
      locals: Locals,
      sourcePaths: List[Path],
      isInit: Boolean
  ) extends FunctionLike(function.params, function.body, locals, sourcePaths) {
    def bind(instance: DObject): Function =
      Function(
        function,
        Env.fromEnclosing(enclosing).define(Lexemes.Self, instance),
        locals,
        sourcePaths,
        isInit
      )
  }

  final case class Lambda(lambda: Expr.Lambda, enclosing: Env, locals: Locals, sourcePaths: List[Path])
      extends FunctionLike(lambda.params, lambda.body, locals, sourcePaths) {
    override def isInit: Boolean = false
  }

  // TODO: See if we need to remove this one for now
  final case class Varargs(enclosing: Env, _call: Token => Call) extends Callable {
    override def arity = Int.MaxValue

    override def call(token: Token): Call = _call(token)
  }

  abstract class CustomCallable(val arity: Int, val enclosing: Env) extends Callable

  def apply(arity: Int, enclosing: Env)(initCall: Call): Callable =
    withLineNo(arity, enclosing)(_ => initCall)

  def withLineNo(arity: Int, enclosing: Env)(initCall: Int => Call): Callable =
    new CustomCallable(arity, enclosing) {
      override def call(token: Token) = initCall(token.line).orElse(_ => Value.None.ok)
    }

  def noArg(enclosing: Env)(initCall: => Result[Value]): Callable =
    Callable(0, enclosing)(_ => initCall)

  def unapply(callable: Callable): Option[(Int, Env, Token => Call)] =
    Some(callable.arity, callable.enclosing, callable.call)

  def unary(enclosing: Env)(call: PartialFunction[Value, Result[Value]]): Callable =
    Callable(1, enclosing) { case arg :: _ => call(arg) }

  def unarySuccess(enclosing: Env)(call: PartialFunction[Value, Value]): Callable =
    unary(enclosing)(call.andThen(value => Result.succeed(value)))
}
