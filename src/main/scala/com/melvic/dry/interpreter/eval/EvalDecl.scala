package com.melvic.dry.interpreter.eval

import com.melvic.dry.ast.Decl._
import com.melvic.dry.ast.{Decl, Stmt}
import com.melvic.dry.interpreter.{Callable, Env, Value}
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.implicits.ToResult

private[eval] trait EvalDecl extends EvalStmt {
  def decl: Evaluate[Decl] = {
    case stmt: StmtDecl => stmtDecl(stmt)
    case let: Let       => Evaluate.let(let)
    case function: Def  => Evaluate.defDecl(function)

    // Not a big fan of this. Declaration statements should be of type StmtDecl,
    // but sometimes I needed to create a statement directly in the statement parser (e.g. a syntactic-sugar),
    // and it didn't sound right to wrap them in StmtDecl, nor would it typecheck.
    // I can solve this issue by either moving the syntactic-sugar here in declaration parser
    // and wrap it in StmtDecl, or remove StmtDecl altogether.
    case stmt: Stmt => Evaluate.stmt(stmt)
  }

  def stmtDecl: Evaluate[Decl] = { case StmtDecl(stmt) =>
    Evaluate.stmt(stmt)
  }

  def let: Evaluate[Let] = {
    def letDecl: Evaluate[LetDecl] = { case LetDecl(name) =>
      env => (Value.Unit, env.define(name.lexeme, Value.None)).ok
    }

    def letInit: Evaluate[LetInit] = { case LetInit(name, init) =>
      Evaluate
        .expr(init)
        .andThen(
          _.flatMap { case (value, env) =>
            (Value.Unit, env.define(name.lexeme, value)).ok
          }
        )
    }

    {
      case let: LetDecl => letDecl(let)
      case let: LetInit => letInit(let)
    }
  }

  def defDecl: Evaluate[Def] = { case function @ Def(name, _, _) =>
    env =>
      lazy val callable: Callable = Callable.Function(function, newEnv)
      lazy val newEnv: Env = env.define(name.lexeme, callable)
      Result.succeed(Value.Unit, newEnv)
  }
}
