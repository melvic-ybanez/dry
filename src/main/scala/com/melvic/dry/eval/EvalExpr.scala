package com.melvic.dry.eval

import com.melvic.dry.Error.RuntimeError
import com.melvic.dry.Result.Result
import com.melvic.dry.Result.impilcits.ToResult
import com.melvic.dry.Token.TokenType
import com.melvic.dry.Value.{Bool, Num, Str, None => VNone}
import com.melvic.dry.ast.Expr
import com.melvic.dry.ast.Expr.{Binary, Grouping, Literal, Unary}
import com.melvic.dry.eval.Evaluate.{Evaluate, isTruthy}
import com.melvic.dry.{Result, Token, Value}

private[eval] trait EvalExpr {
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
          Result.fromOption(
            operand.toNum.map(num => Num(-num.value)),
            RuntimeError.invalidOperand(operator, "number" :: Nil)
          )
        case TokenType.Not => Bool(!isTruthy(operand)).ok
        case _             => VNone.ok
      }
    }
  }

  def binary: Evaluate[Binary] = { case Binary(leftTree, operator @ Token(operatorType, _, _), rightTree) =>
    def fromValueOperands(left: Value, right: Value): Result[Value] = {
      def binary[O, V](fold: (Double, Double) => O, toValue: O => V): Result[V] =
        Result.fromOption(
          for {
            leftNum  <- left.toNum
            rightNum <- right.toNum
          } yield toValue(fold(leftNum.value, rightNum.value)),
          RuntimeError.invalidOperands(operator, "number" :: Nil)
        )

      def combine(f: (Double, Double) => Result[Double]): Result[Num] =
        binary(f, (result: Result[Double]) => result.map(Num)).flatten

      def combineUnsafe(f: (Double, Double) => Double): Result[Num] =
        combine((x, y) => f(x, y).ok)

      def compare(f: (Double, Double) => Boolean): Result[Bool] =
        binary(f, Bool)

      def bitwise(f: (Long, Long) => Long): Result[Num] =
        combineUnsafe { case (x, y) => f(x.toLong, y.toLong) }

      operatorType match {
        case TokenType.Plus =>
          (left, right) match {
            case (Num(l), Num(r)) => Num(l + r).ok
            case (Str(l), Str(r)) => Str(l + r).ok
            case _ =>
              val error = RuntimeError.invalidOperands(operator, List("number", "string"))
              Result.fail(error)
          }
        case TokenType.Minus => combineUnsafe(_ - _)
        case TokenType.Star  => combineUnsafe(_ * _)
        case TokenType.Slash =>
          combine {
            case (_, 0) => Result.fail(RuntimeError.divisionByZero(operator))
            case (x, y) => (x / y).ok
          }
        case TokenType.Modulo       => combineUnsafe(_ % _)
        case TokenType.BAnd         => bitwise(_ & _)
        case TokenType.BOr          => bitwise(_ | _)
        case TokenType.BXor         => bitwise(_ ^ _)
        case TokenType.LeftShift    => bitwise(_ << _)
        case TokenType.RightShift   => bitwise(_ >> _)
        case TokenType.URightShift  => bitwise(_ >>> _)
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

}
