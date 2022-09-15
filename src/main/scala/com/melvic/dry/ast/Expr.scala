package com.melvic.dry.ast

import com.melvic.dry.Token

sealed trait Expr

object Expr {
  sealed trait Literal extends Expr

  object Literal {
    final case class Number(value: Double) extends Literal
    final case class Str(value: String)    extends Literal
    case object True                       extends Literal
    case object False                      extends Literal
    case object None                       extends Literal
  }

  final case class Grouping(expr: Expr)                             extends Expr
  final case class Unary(operator: Token, operand: Expr)            extends Expr
  final case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
  final case class Variable(token: Token)                           extends Expr
  final case class Assignment(name: Token, value: Expr)             extends Expr

  final case class Logical(left: Expr, operator: Token, right: Expr) extends Expr

  final case class Call(callee: Expr, arguments: List[Expr], paren: Token) extends Expr
}
