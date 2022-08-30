package com.melvic.dry

import com.melvic.dry.Expr.{Binary, Grouping, Literal, Unary}
import com.melvic.dry.Token.TokenType
import com.melvic.dry.Value.implicits._
import com.melvic.dry.Value.{Bool, Num, Str, None => VNone}

object Evaluate {
  // TODO: Add error channel
  type Evaluate[A <: Expr] = A => Value

  def expr: Evaluate[Expr] = {
    case literal: Literal => Evaluate.literal(literal)
    case Grouping(expr)   => Evaluate.expr(expr)
  }

  def unary: Evaluate[Unary] = { case Unary(Token(operatorType, _, _), operandTree) =>
    val operand = Evaluate.expr(operandTree)
    operatorType match {
      case TokenType.Minus => Num(-operand.toNum.value)
      case TokenType.Not   => Bool(!isTruthy(operand).value)
      case _               => VNone
    }
  }

  def binary: Evaluate[Binary] = { case Binary(leftTree, Token(operatorType, _, _), rightTree) =>
    val left = Evaluate.expr(leftTree)
    val right = Evaluate.expr(rightTree)
    operatorType match {
      case TokenType.Plus =>
        (left, right) match {
          case (Num(l), Num(r)) => Num(l + r)
          case (Str(l), Str(r)) => Str(l + r)
          case _                => VNone
        }
      case TokenType.Minus => left + right
      case TokenType.Star  => left * right
      case TokenType.Slash => left / right
      case _               => VNone
    }
  }

  def literal: Evaluate[Literal] = {
    case Literal.True          => Bool(true)
    case Literal.False         => Bool(false)
    case Literal.None          => VNone
    case Literal.Number(value) => Num(value)
    case Literal.Str(string)   => Str(string)
  }

  def isTruthy(value: Value): Bool =
    value match {
      case VNone         => Bool(false)
      case Str("")       => Bool(false)
      case boolean: Bool => boolean
      case _             => Bool(true)
    }
}
