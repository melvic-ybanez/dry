package com.melvic.dry.ast

import com.melvic.dry.Token

sealed trait Expr

object Expr {
  sealed trait Literal extends Expr

  object Literal {
    final case class Number(value: Double) extends Literal
    final case class Str(value: String) extends Literal
    case object True extends Literal
    case object False extends Literal
    case object None extends Literal
  }

  final case class Grouping(expr: Expr) extends Expr
  final case class Unary(operator: Token, operand: Expr) extends Expr
  final case class Binary(left: Expr, operator: Token, right: Expr) extends Expr

  sealed trait Operator extends Expr

  object Operator {
    case object EqualEqual extends Operator
    case object NotEqual extends Operator
    case object LessThan extends Operator
    case object LessThanEqual extends Operator
    case object GreaterThan extends Operator
    case object GreaterThanEqual extends Operator
    case object Plus extends Operator
    case object Minus extends Operator
    case object Multiply extends Operator
    case object Divide extends Operator
    case object Modulo extends Operator
  }
}
