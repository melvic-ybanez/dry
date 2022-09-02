package com.melvic.dry

import com.melvic.dry.Error.ParseError
import com.melvic.dry.Expr.{Binary, Grouping, Literal, Unary}
import com.melvic.dry.Parser.{ParseResult, Parsed}
import com.melvic.dry.Result.Result
import com.melvic.dry.Token.TokenType

import scala.util.chaining._

final case class Parser(tokens: List[Token], current: Int) {
  def parse: Result[Expr] = expression.map(_._1)

  def expression: ParseResult[Expr] =
    equality

  def equality: ParseResult[Expr] =
    leftAssociativeBinary(_.comparison, TokenType.NotEqual, TokenType.EqualEqual)

  /**
   * Parses a left-associative binary expression given a set of valid operators. The matched operator and
   * right-hand operand can appear at-least zero times.
   */
  def leftAssociativeBinary(operand: Parser => ParseResult[Expr], operators: TokenType*): ParseResult[Expr] =
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

  def comparison: ParseResult[Expr] =
    leftAssociativeBinary(
      _.term,
      TokenType.Greater,
      TokenType.GreaterEqual,
      TokenType.Less,
      TokenType.LessEqual
    )

  def term: ParseResult[Expr] =
    leftAssociativeBinary(_.factor, TokenType.Minus, TokenType.Plus)

  def factor: ParseResult[Expr] =
    leftAssociativeBinary(_.unary, TokenType.Slash, TokenType.Star)

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

  def matchAny(tokenTypes: TokenType*): Option[Parser] =
    matchAnyWith(tokenType => tokenTypes.contains(tokenType))

  def matchAnyWith(predicate: PartialFunction[TokenType, Boolean]): Option[Parser] =
    checkWith(predicate).pipe {
      case true  => Some(advance._2)
      case false => None
    }

  def checkWith(f: PartialFunction[TokenType, Boolean]): Boolean =
    if (isAtEnd) false
    else f.applyOrElse(peek.tokenType, (_: TokenType) => false)

  def check(tokenType: TokenType): Boolean =
    checkWith(_ == tokenType)

  def advance: Parsed[Token] =
    if (isAtEnd) (previous, this)
    else {
      val parser = copy(current = current + 1)
      (parser.previous, parser)
    }

  def consume(tokenType: TokenType, expected: String): ParseResult[Token] =
    if (check(tokenType)) Result.success(advance)
    else Result.fail(ParseError.expected(peek, expected))

  def isAtEnd: Boolean =
    peek.tokenType == TokenType.Eof

  def peek: Token =
    tokens(current)

  def previous: Token =
    tokens(current - 1)
}

object Parser {
  type Parsed[A]      = (A, Parser)
  type ParseResult[A] = Result[Parsed[A]]

  def fromTokens(tokens: List[Token]): Parser =
    Parser(tokens, 0)
}
