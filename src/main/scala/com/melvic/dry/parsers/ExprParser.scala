package com.melvic.dry.parsers

import com.melvic.dry.Error.ParseError
import com.melvic.dry.Result
import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Expr
import com.melvic.dry.ast.Expr.{Binary, Grouping, Literal, Unary}
import com.melvic.dry.parsers.Parser.ParseResult

import scala.util.chaining.scalaUtilChainingOps

private[parsers] trait ExprParser { _: Parser =>
  def expression: ParseResult[Expr] =
    equality

  def equality: ParseResult[Expr] =
    leftAssocBinary(_.comparison, TokenType.NotEqual, TokenType.EqualEqual)

  def comparison: ParseResult[Expr] =
    leftAssocBinary(
      _.term,
      TokenType.Greater,
      TokenType.GreaterEqual,
      TokenType.Less,
      TokenType.LessEqual
    )

  def term: ParseResult[Expr] =
    leftAssocBinary(_.factor, TokenType.Minus, TokenType.Plus)

  def factor: ParseResult[Expr] =
    leftAssocBinary(_.unary, TokenType.Slash, TokenType.Star, TokenType.Modulo)

  def unary: ParseResult[Expr] =
    matchAny(TokenType.Not, TokenType.Minus)
      .map { parser =>
        val operator = parser.previous
        parser.unary.map { case (right, newParser) =>
          (Unary(operator, right), newParser)
        }
      }
      .getOrElse(primary)

  def primary: ParseResult[Expr] =
    matchAny(TokenType.False)
      .map((Literal.False, _))
      .orElse(matchAny(TokenType.True).map((Literal.True, _)))
      .orElse(matchAny(TokenType.None).map((Literal.None, _)))
      .orElse {
        matchAnyWith {
          case TokenType.Number(_) => true
          case TokenType.Str(_)    => true
        }.map { parser =>
          (parser.previous.tokenType match {
            case TokenType.Number(number) => Literal.Number(number)
            case TokenType.Str(string)    => Literal.Str(string)
          }).pipe((_, parser))
        }
      }
      .map(Result.success)
      .getOrElse(
        matchAny(TokenType.LeftParen)
          .fold[ParseResult[Expr]](Result.fail(ParseError.expected(peek, "expression"))) { parser =>
            parser.expression.flatMap { case (expr, newParser) =>
              newParser.consume(TokenType.RightParen, ")").map { case (_, parser) =>
                (Grouping(expr), parser)
              }
            }
          }
      )

  /**
   * Parses a left-associative binary expression given a set of valid operators. The matched operator and
   * right-hand operand can appear at-least zero times.
   */
  private def leftAssocBinary(
      operand: Parser => ParseResult[Expr],
      operators: TokenType*
  ): ParseResult[Expr] =
    operand(this).flatMap { case (expr, parser) =>
      def recurse(expr: Expr, parser: Parser): ParseResult[Expr] =
        parser
          .matchAny(operators: _*)
          .map { parser =>
            val operator = parser.previous
            operand(parser).flatMap { case (right, newParser) =>
              // recursively check if another operator from the given set, followed by
              // the same operand, is found again
              recurse(Binary(expr, operator, right), newParser)
            }
          }
          .getOrElse(Result.success(expr, parser))

      recurse(expr, parser)
    }
}
