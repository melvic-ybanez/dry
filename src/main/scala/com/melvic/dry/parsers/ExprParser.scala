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

  /**
   * {{{<assignment> ::= (<call> ".")? <identifier> "=" <assignment> | <lambda>}}}
   */
  def assignment: ParseResult[Expr] = {
    // parse the left value as a lambda to cover all possible expression (including chained `get`s)
    lambda.flatMap { case Step(lValue, parser) =>
      // if the resulting left value is not followed by an equal sign, return it
      parser.matchAny(TokenType.Equal).fold(ParseResult.succeed(lValue, parser)) { parser =>
        // otherwise, check if it's a valid assignment target
        parser.assignment.flatMap { case Step(rValue, parser) =>
          lValue match {
            case Variable(name) => ParseResult.succeed(Assignment(name, rValue), parser)
            case Get(obj, name) => ParseResult.succeed(Expr.Set(obj, name, lValue), parser)
            case _ => ParseResult.fail(ParseError.invalidAssignmentTarget(parser.previous), parser)
          }
        }
      }
    }
  }

  /**
   * {{{<lambda> ::= "Lambda" <params> <function-body> | <or>}}}
   */
  def lambda: ParseResult[Expr] =
    matchAny(TokenType.Lambda)
      .fold(or) { parser =>
        for {
          params <- parser.params
          body   <- params.functionBody("lambda")
        } yield Step(Lambda(params.value, body.value.declarations), body.next)
      }

  /**
   * {{{<or> ::= <and> ("or" <and>)*}}}
   */
  def or: ParseResult[Expr] =
    leftAssocLogical(_.and, TokenType.Or)

  /**
   * {{{<and> ::= <equality> ("and" <equality>)*}}}
   */
  def and: ParseResult[Expr] =
    leftAssocLogical(_.equality, TokenType.And)

  /**
   * {{{<equality> ::= <comparison> ("!=" | "==" <comparison>)*}}}
   */
  def equality: ParseResult[Expr] =
    leftAssocBinary(_.comparison, TokenType.NotEqual, TokenType.EqualEqual)

  /**
   * {{{<comparison> ::= <term> (">" | ">=" | "<" | "<=" <term>)*}}}
   */
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
      .getOrElse(call)

  def call: ParseResult[Expr] = {
    def parenCall(callee: Expr, parser: Parser): ParseResult[Expr] = {
      def recurse(args: List[Expr], parser: Parser): ParseResult[List[Expr]] =
        parser
          .matchAny(TokenType.Comma)
          .fold(ParseResult.succeed(args.reverse, parser)) { parser =>
            parser.expression.flatMap { case Step(arg, parser) =>
              recurse(arg :: args, parser)
            }
          }

      val result =
        if (!parser.check(TokenType.RightParen))
          parser.expression.flatMap(step => recurse(step.value :: Nil, step.next))
        else ParseResult.succeed(Nil, parser)

      result.parser
        .consume(TokenType.RightParen, ")", "function call arguments")
        .flatMap(step => result.mapValue(Call(callee, _, step.value)).mapParser(_ => step.next))
    }

    primary.flatMap {
      // Recursively checks if the expression is being called, or a property access is being invoked
      def checkForCalls(step: Step[Expr]): ParseResult[Expr] = {
        def propAccess(expr: Expr, next: Parser) =
          next.consume(TokenType.Identifier, "property name", ".").mapValue(Get(expr, _))

        step.next
          .matchAny(TokenType.LeftParen)
          .fold(
            step.next
              .matchAny(TokenType.Dot)
              .fold(ParseResult.fromStep(step))(propAccess(step.value, _).flatMap(checkForCalls))
          )(parenCall(step.value, _).flatMap(checkForCalls))
      }

      checkForCalls
    }
  }

  def primary: ParseResult[Expr] =
    matchAny(TokenType.False)
      .map(Step(Literal.False, _))
      .orElse(matchAny(TokenType.True).map(Step(Literal.True, _)))
      .orElse(matchAny(TokenType.None).map(Step(Literal.None, _)))
      .orElse {
        matchAnyWith {
          case TokenType.Number(_) => true
          case TokenType.Str(_)    => true
        }.map { parser =>
          (parser.previous.tokenType match {
            case TokenType.Number(number) => Literal.Number(number)
            case TokenType.Str(string)    => Literal.Str(string)
          }).pipe(Step(_, parser))
        }
      }
      .orElse(matchAny(TokenType.Identifier).map(p => Step(Variable(p.previous), p)))
      .map(_.toParseResult)
      .getOrElse(
        matchAny(TokenType.LeftParen)
          .fold[ParseResult[Expr]](ParseResult.fail(ParseError.expected(peek, "expression", "("), this)) {
            parser =>
              parser.expression.flatMap { case Step(expr, newParser) =>
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
   * right-hand operand can appear at-least zero times. It is generally in form of
   * {{{<operand> (<operators> <operand>)*}}}
   * @param operand
   *   the higher precedence expression
   */
  private def leftAssocBinaryWith[E <: Expr](
      operand: Parser => ParseResult[Expr],
      toBinary: (Expr, Token, Expr) => E,
      operators: TokenType*
  ): ParseResult[Expr] =
    operand(this).flatMap { case Step(expr, parser) =>
      def recurse(expr: Expr, parser: Parser): ParseResult[Expr] =
        parser
          .matchAny(operators: _*)
          .map { parser =>
            val operator = parser.previous
            operand(parser).flatMap { case Step(right, newParser) =>
              // recursively check if another operator from the given set, followed by
              // the same operand, is found again
              recurse(toBinary(expr, operator, right), newParser)
            }
          }
          .getOrElse(Step(expr, parser).toParseResult)

      recurse(expr, parser)
    }
}
