package com.melvic.dry.interpreter.values

import com.melvic.dry.Token
import com.melvic.dry.ast.Decl.Def
import com.melvic.dry.ast.Stmt.BlockStmt
import com.melvic.dry.ast.{Decl, Expr}
import com.melvic.dry.interpreter.Env
import com.melvic.dry.interpreter.Env.Keys.LineNumber
import com.melvic.dry.interpreter.eval.Evaluate
import com.melvic.dry.interpreter.values.Callable.Call
import com.melvic.dry.interpreter.values.Value.Returned
import com.melvic.dry.lexer.Lexemes
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.Result

private[interpreter] trait Callable extends Value {
  def arity: Int

  def call: Call

  def enclosing: Env

  def callWithPos(token: Token): Call = {
    local.define(LineNumber, Value.Num(token.line))
    call
  }

  protected val local: Env = Env.fromEnclosing(enclosing)

  def lineNumber: Int = local.at(0, LineNumber).flatMap(_.toNum.map(_.value.toInt)).getOrElse(0)
}

object Callable {
  type Call = List[Value] => Result[Value]

  abstract class FunctionLike(val params: List[Token], val body: List[Decl]) extends Callable {
    def isInit: Boolean

    override def arity = params.size

    override def call: Call = { args =>
      val env = params.zip(args).foldLeft(local) { case (env, (param, arg)) =>
        env.define(param.lexeme, arg)
      }
      Evaluate
        .blockStmt(BlockStmt(body))(env)
        .map(value => if (isInit) init.getOrElse(value) else value)
        .map {
          case Returned(value) => if (isInit) init.getOrElse(Value.None) else value
          case value           => value
        }
    }

    lazy val init: Option[Value] = enclosing.at(0, Lexemes.Init)
  }

  final case class Function(function: Def, enclosing: Env, isInit: Boolean)
      extends FunctionLike(function.params, function.body) {
    def bind(instance: DObject): Function =
      Function(function, Env.fromEnclosing(enclosing).define(Lexemes.Self, instance), isInit)
  }

  final case class Lambda(lambda: Expr.Lambda, enclosing: Env)
      extends FunctionLike(lambda.params, lambda.body) {
    override def isInit: Boolean = false
  }

  final case class Varargs(enclosing: Env, call: Call) extends Callable {
    override def arity = Int.MaxValue
  }

  abstract class CustomCallable(val arity: Int, val enclosing: Env) extends Callable

  def apply(arity: Int, enclosing: Env)(initCall: Call): Callable =
    withLineNo(arity, enclosing)(_ => initCall)

  def withLineNo(arity: Int, enclosing: Env)(initCall: Int => Call): Callable =
    new CustomCallable(arity, enclosing) {
      override def call = initCall(lineNumber)
    }

  def noArg(enclosing: Env)(initCall: => Result[Value]): Callable =
    Callable(0, enclosing)(_ => initCall)

  def unapply(callable: Callable): Option[(Int, Env, Token => Call)] =
    Some(callable.arity, callable.enclosing, callable.callWithPos)

  def unary(enclosing: Env)(call: Value => Result[Value]): Callable =
    Callable(1, enclosing) { case arg :: _ => call(arg) }

  def unarySuccess(enclosing: Env)(call: Value => Value): Callable =
    unary(enclosing)(call.andThen(value => Result.succeed(value)))
}
