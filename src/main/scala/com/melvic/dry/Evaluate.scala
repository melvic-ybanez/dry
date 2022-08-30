package com.melvic.dry

import com.melvic.dry.Expr.{Binary, Grouping, Literal, Unary}
import com.melvic.dry.Result.Result
import com.melvic.dry.Result.impilcits.ResultOps
import com.melvic.dry.Token.TokenType
import com.melvic.dry.Value.{Bool, Num, Str, None => VNone}

object Evaluate {
  type Evaluate[A <: Expr] = A => Result[Value]

  def expr: Evaluate[Expr] = {
    case literal: Literal => Evaluate.literal(literal)
    case Grouping(expr)   => Evaluate.expr(expr)
    case unary: Unary     => Evaluate.unary(unary)
    case binary: Binary   => Evaluate.binary(binary)
  }

  def unary: Evaluate[Unary] = { case Unary(operator @ Token(operatorType, _, _), operandTree) =>
    Evaluate.expr(operandTree).flatMap { operand =>
      operatorType match {
        case TokenType.Minus =>
          Result.fromOption(operand.toNum.map(num => Num(-num.value)), Error.numberOperand(operator))
        case TokenType.Not => Bool(!isTruthy(operand)).ok
        case _             => VNone.ok
      }
    }
  }

  def binary: Evaluate[Binary] = { case Binary(leftTree, operator @ Token(operatorType, _, _), rightTree) =>
    def fromValueOperands(left: Value, right: Value): Result[Value] = {
      def binary[O, V <: Value](fold: (Double, Double) => O, toValue: O => V): Result[V] =
        Result.fromOption(
          for {
            leftNum  <- left.toNum
            rightNum <- right.toNum
          } yield toValue(fold(leftNum.value, rightNum.value)),
          Error.numberOperands(operator)
        )

      def combine(f: (Double, Double) => Double): Result[Num] =
        binary(f, Num)

      def compare(f: (Double, Double) => Boolean): Result[Bool] =
        binary(f, Bool)

      operatorType match {
        case TokenType.Plus =>
          (left, right) match {
            case (Num(l), Num(r)) => Num(l + r).ok
            case (Str(l), Str(r)) => Str(l + r).ok
            case _ =>
              val error = Error.runtimeError(operator, "Operands must be either both numbers or both strings")
              Result.fail(error)
          }
        case TokenType.Minus        => combine(_ - _)
        case TokenType.Star         => combine(_ * _)
        case TokenType.Slash        => combine(_ / _)
        case TokenType.Greater      => compare(_ > _)
        case TokenType.GreaterEqual => compare(_ >= _)
        case TokenType.Less         => compare(_ < _)
        case TokenType.LessEqual    => compare(_ <= _)
        case TokenType.NotEqual     => compare(_ != _)
        case TokenType.EqualEqual   => compare(_ == _)
        case _                      => VNone.ok
      }
    }

    for {
      left   <- Evaluate.expr(leftTree)
      right  <- Evaluate.expr(rightTree)
      result <- fromValueOperands(left, right)
    } yield result
  }

  def literal: Evaluate[Literal] = {
    case Literal.True          => Bool(true).ok
    case Literal.False         => Bool(false).ok
    case Literal.None          => VNone.ok
    case Literal.Number(value) => Num(value).ok
    case Literal.Str(string)   => Str(string).ok
  }

  def isTruthy(value: Value): Boolean =
    value match {
      case VNone       => false
      case Str("")     => false
      case Bool(value) => value
      case _           => true
    }
}
