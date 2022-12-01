package com.melvic.dry.aux

import com.melvic.dry.Token
import com.melvic.dry.ast.{Decl, Expr, Stmt}
import com.melvic.dry.interpreter.{Env, Value}

object Show {
  type Show[-A] = A => String

  implicit class ShowInterpolator(sc: StringContext) {
    def show(args: Any*): String = {
      val newArgs = args.map {
        case expr: Expr   => Expr.show(expr)
        case stmt: Stmt   => Stmt.show(stmt)
        case decl: Decl   => Decl.show(decl)
        case value: Value => Value.show(value)
        case token: Token => Token.show(token)
        case env: Env     => Env.show(env)
        case etc          => etc
      }
      sc.s(newArgs: _*)
    }
  }
}
