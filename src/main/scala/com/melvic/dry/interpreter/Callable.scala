package com.melvic.dry.interpreter

import com.melvic.dry.ast.Decl.Def
import com.melvic.dry.ast.Stmt.BlockStmt
import com.melvic.dry.interpreter.Env.LocalEnv
import com.melvic.dry.interpreter.Value.Returned
import com.melvic.dry.interpreter.eval.Evaluate

private[interpreter] trait Callable extends Value {
  def arity: Int

  def call: Evaluate[List[Value]]
}

object Callable {
  final case class Function(function: Def) extends Callable {
    override def arity = function.params.size

    override def call: Evaluate[List[Value]] = { args => enclosingEnv =>
      val env = function.params.zip(args).foldLeft(Env.localEnv(enclosingEnv)) { case (env, (param, arg)) =>
        env.define(param.lexeme, arg)
      }
      Evaluate.blockStmt(BlockStmt(function.body))(env).map {
        case (Returned(value), LocalEnv(_, enclosing)) => (value, enclosing)
        case (value, LocalEnv(_, enclosing))           => (value, enclosing)
        case result                                    => result
      }
    }
  }

  def apply(initArity: Int, initCall: Evaluate[List[Value]]): Callable =
    new Callable {
      override def arity = initArity

      override def call = initCall
    }

  def unapply(callable: Callable): Option[(Int, Evaluate[List[Value]])] =
    Some(callable.arity, callable.call)

  def unary(call: Evaluate[Value]): Callable =
    Callable(1, { case arg :: _ => call(arg) })
}
