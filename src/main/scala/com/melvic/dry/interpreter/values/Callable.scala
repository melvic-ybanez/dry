package com.melvic.dry.interpreter.values

import com.melvic.dry.Token
import com.melvic.dry.ast.Decl.Def
import com.melvic.dry.ast.Stmt.BlockStmt
import com.melvic.dry.ast.{Decl, Expr}
import com.melvic.dry.interpreter.Env
import com.melvic.dry.interpreter.eval.Evaluate
import com.melvic.dry.interpreter.values.Callable.Call
import com.melvic.dry.interpreter.values.Value.Returned
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.Result

private[interpreter] trait Callable extends Value {
  def arity: Int

  def call: Call

  def enclosing: Env
}

object Callable {
  type Call = List[Value] => Result[Value]

  abstract class FunctionLike(val params: List[Token], val body: List[Decl]) extends Callable {
    override def arity = params.size

    override def call: Call = { args =>
      val env = params.zip(args).foldLeft(Env.fromEnclosing(enclosing)) { case (env, (param, arg)) =>
        env.define(param.lexeme, arg)
      }
      Evaluate.blockStmt(BlockStmt(body))(env).map {
        case Returned(value) => value
        case value           => value
      }
    }
  }

  final case class Function(function: Def, enclosing: Env)
      extends FunctionLike(function.params, function.body)

  final case class Lambda(lambda: Expr.Lambda, enclosing: Env)
      extends FunctionLike(lambda.params, lambda.body)

  def apply(initArity: Int, initEnclosing: Env)(initCall: Call): Callable =
    new Callable {
      override def enclosing = initEnclosing

      override def arity = initArity

      override def call = initCall
    }

  def unapply(callable: Callable): Option[(Int, Env, Call)] =
    Some(callable.arity, callable.enclosing, callable.call)

  def unary(enclosing: Env)(call: Value => Result[Value]): Callable =
    Callable(1, enclosing) { case arg :: _ => call(arg) }

  def unarySuccess(enclosing: Env)(call: Value => Value): Callable =
    unary(enclosing)(call.andThen(value => Result.succeed(value)))
}
