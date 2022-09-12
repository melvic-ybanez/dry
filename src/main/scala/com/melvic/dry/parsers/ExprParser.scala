package com.melvic.dry.parsers

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Expr
import com.melvic.dry.ast.Expr._
import com.melvic.dry.result.Failure.ParseError

import scala.util.chaining.scalaUtilChainingOps

private[parsers] trait ExprParser { _: Parser =>
  def expression: ParseResult[Expr] =
    assignment

  def assignment: ParseResult[Expr] =
    or.flatMap { case State(lValue, parser) =>
      parser.matchAny(TokenType.Equal).fold(ParseResult.succeed(lValue, parser)) { parser =>
        val equals = parser.previous
        parser.assignment.flatMap { case State(rValue, parser) =>
          lValue match {
            case Variable(name) => ParseResult.succeed(Assignment(name, rValue), parser)
            case _              => ParseResult.fail(ParseError.invalidAssignmentTarget(equals), parser)
          }
        }
      }
    }

  def or: ParseResult[Expr] =
    leftAssocLogical(_.and, TokenType.Or)

  def and: ParseResult[Expr] =
    leftAssocLogical(_.equality, TokenType.And)

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
          .fold[ParseResult[Expr]](ParseResult.fail(ParseError.expected(peek, "expression", "("), this)) {
            parser =>
              parser.expression.flatMap { case State(expr, newParser) =>
                newParser.consume(TokenType.RightParen, ")", "expression").mapValue(_ => Grouping(expr))
              }
          }
      )

  /**
   * Like [[leftAssocBinaryWith]], but is specific to non-logical binary operators.
   */
  private def leftAssocBinary(
      operand: Parser => ParseResult[Expr],
      operators: TokenType*
  ): ParseResult[Expr] =
    leftAssocBinaryWith(operand, Binary, operators: _*)

  /**
   * Like [[leftAssocBinaryWith]], but is specific to logical binary operators.
   */
  private def leftAssocLogical(
      operand: Parser => ParseResult[Expr],
      operators: TokenType*
  ): ParseResult[Expr] =
    leftAssocBinaryWith(operand, Logical, operators: _*)

  /**
   * Parses a left-associative binary expression given a set of valid operators. The matched operator and
   * right-hand operand can appear at-least zero times.
   */
  private def leftAssocBinaryWith[E <: Expr](
      operand: Parser => ParseResult[Expr],
      toBinary: (Expr, Token, Expr) => E,
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
              recurse(toBinary(expr, operator, right), newParser)
            }
          }
          .getOrElse(State(expr, parser).toParseResult)

      recurse(expr, parser)
    }
}
