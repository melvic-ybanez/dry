package com.melvic.dry.interpreter

import com.melvic.dry.ast.Decl.Def
import com.melvic.dry.ast.Stmt.BlockStmt
import com.melvic.dry.interpreter.Callable.Call
import com.melvic.dry.interpreter.Value.Returned
import com.melvic.dry.interpreter.eval.{EvalOut, Evaluate}
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.Result

private[interpreter] trait Callable extends Value {
  def arity: Int

  def call: Call

  def enclosing: Env
}

object Callable {
  type Call = List[Value] => Result[Value]

  abstract class Function(val function: Def) extends Callable {
    override def arity = function.params.size

    override def call: Call = { args =>
      val env = function.params.zip(args).foldLeft(Env.fromEnclosing(enclosing)) { case (env, (param, arg)) =>
        env.define(param.lexeme, arg)
      }
      Evaluate.blockStmt(BlockStmt(function.body))(env).map {
        case (Returned(value), _) => value
        case (value, _)           => value
      }
    }
  }

  object Function {
    def apply(function: Def, initEnclosing: => Env): Function =
      new Function(function) {
        override def enclosing = initEnclosing
      }

    def unapply(function: Function): Option[(Def, Env)] =
      Some(function.function, function.enclosing)
  }

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
