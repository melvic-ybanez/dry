package com.melvic.dry.parsers

import com.melvic.dry.Failure.ParseError
import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Expr
import com.melvic.dry.ast.Expr._

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
    leftAssocBinary(
      _.factor,
      TokenType.Minus,
      TokenType.Plus,
      TokenType.BAnd,
      TokenType.BOr,
      TokenType.BXor,
      TokenType.LeftShift,
      TokenType.RightShift,
      TokenType.URightShift
    )

  def factor: ParseResult[Expr] =
    leftAssocBinary(_.unary, TokenType.Slash, TokenType.Star, TokenType.Modulo)

  def unary: ParseResult[Expr] =
    matchAny(TokenType.Not, TokenType.Minus)
      .map { parser =>
        val operator = parser.previous
        parser.unary.mapValue(Unary(operator, _))
      }
      .getOrElse(primary)

  def primary: ParseResult[Expr] =
    matchAny(TokenType.False)
      .map(State(Literal.False, _))
      .orElse(matchAny(TokenType.True).map(State(Literal.True, _)))
      .orElse(matchAny(TokenType.None).map(State(Literal.None, _)))
      .orElse {
        matchAnyWith {
          case TokenType.Number(_) => true
          case TokenType.Str(_)    => true
        }.map { parser =>
          (parser.previous.tokenType match {
            case TokenType.Number(number) => Literal.Number(number)
            case TokenType.Str(string)    => Literal.Str(string)
          }).pipe(State(_, parser))
        }
      }
      .orElse(matchAny(TokenType.Identifier).map(p => State(Variable(p.previous), p)))
      .map(_.toParseResult)
      .getOrElse(
        matchAny(TokenType.LeftParen)
          .fold[ParseResult[Expr]](ParseResult.fail(ParseError.expected(peek, "expression"), this)) {
            parser =>
              parser.expression.flatMap { case State(expr, newParser) =>
                newParser.consume(TokenType.RightParen, ")").mapValue(_ => Grouping(expr))
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
    operand(this).flatMap { case State(expr, parser) =>
      def recurse(expr: Expr, parser: Parser): ParseResult[Expr] =
        parser
          .matchAny(operators: _*)
          .map { parser =>
            val operator = parser.previous
            operand(parser).flatMap { case State(right, newParser) =>
              // recursively check if another operator from the given set, followed by
              // the same operand, is found again
              recurse(Binary(expr, operator, right), newParser)
            }
          }
          .getOrElse(State(expr, parser).toParseResult)

      recurse(expr, parser)
    }
}
