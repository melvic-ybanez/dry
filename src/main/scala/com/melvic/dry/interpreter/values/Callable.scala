package com.melvic.dry.interpreter.values

import com.melvic.dry.Token
import com.melvic.dry.ast.Decl.Def
import com.melvic.dry.ast.Stmt.BlockStmt
import com.melvic.dry.ast.{Decl, Expr}
import com.melvic.dry.interpreter.Env
import com.melvic.dry.interpreter.eval.Evaluate
import com.melvic.dry.interpreter.values.Callable.Call
import com.melvic.dry.interpreter.values.Value.Returned
import com.melvic.dry.lexer.Lexemes
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.Result

private[interpreter] trait Callable extends Value {
  def arity: Int

  def call(token: Token): Call

  def enclosing: Env
}

object Callable {
  type Call = List[Value] => Result[Value]

  val LineNumber = "__line_number__"

  abstract class FunctionLike(val params: List[Token], val body: List[Decl]) extends Callable {
    def isInit: Boolean

    override def arity = params.size

    override def call(token: Token): Call = { args =>
      val initEnv = Env.fromEnclosing(enclosing).define(LineNumber, Value.Num(token.line))
      val env = params.zip(args).foldLeft(initEnv) { case (env, (param, arg)) =>
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

  trait Varargs extends Callable

  def apply(initArity: Int, initEnclosing: Env)(initCall: Call): Callable =
    new Callable {
      override def enclosing = initEnclosing

      override def arity = initArity

      override def call(token: Token) = initCall
    }

  def unapply(callable: Callable): Option[(Int, Env, Token => Call)] =
    Some(callable.arity, callable.enclosing, callable.call)

  def unary(enclosing: Env)(call: Value => Result[Value]): Callable =
    Callable(1, enclosing) { case arg :: _ => call(arg) }

  def unarySuccess(enclosing: Env)(call: Value => Value): Callable =
    unary(enclosing)(call.andThen(value => Result.succeed(value)))

  def varargs(env: Env)(initCall: Call): Varargs = new Varargs {
    override def arity = Int.MaxValue

    override def call(token: Token) = initCall

    override def enclosing = env
  }
}
