package com.melvic.dry.parsers

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Expr
import com.melvic.dry.ast.Expr._
import com.melvic.dry.ast.Stmt.ReturnStmt
import com.melvic.dry.lexer.Lexemes
import com.melvic.dry.result.Failure.ParseError

import scala.annotation.nowarn

//noinspection ScalaWeakerAccess
private[parsers] trait ExprParser { _: Parser =>

  /**
   * {{{<expression> ::= <assignment> | <lambda>}}}
   */
  def expression: ParseResult[Expr] =
    assignment

  /**
   * {{{<assignment> ::= <call> "=" <expression>}}}
   */
  def assignment: ParseResult[Expr] = {
    // parse the left value as a lambda to cover all possible expression (including chained `get`s)
    lambda.flatMap { case Step(lValue, parser) =>
      // if the resulting left value is not followed by an equal sign, return it
      parser.matchAny(TokenType.Equal).fold(ParseResult.succeed(lValue, parser)) { parser =>
        // otherwise, check if it's a valid assignment target
        parser.expression.flatMap { case Step(rValue, parser) =>
          lValue match {
            case Variable(name)      => ParseResult.succeed(Assignment(name, rValue), parser)
            case Get(obj, name)      => ParseResult.succeed(Set(obj, name, rValue), parser)
            case IndexGet(obj, name) => ParseResult.succeed(IndexSet(obj, name, rValue), parser)
            case _ => ParseResult.fail(ParseError.invalidAssignmentTarget(parser.previousToken), parser)
          }
        }
      }
    }
  }

  /**
   * {{{<lambda> ::= "lambda" <params> <function-body> | <or>}}}
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

  /**
   * {{{<term> ::= <factor> ("-" | "+" | "&" | "|" | "^" | "<<" | ">>" | ">>>" <factor>)*}}}
   */
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

  /**
   * {{{<factor> ::= <unary> ("/" | "*" | "%" <unary>)*}}}
   */
  def factor: ParseResult[Expr] =
    leftAssocBinary(_.unary, TokenType.Slash, TokenType.Star, TokenType.Modulo)

  /**
   * {{{<unary> ::= ("!" | "-" | "+" | "not")* <call>}}}
   */
  def unary: ParseResult[Expr] =
    matchAny(TokenType.Not, TokenType.Minus, TokenType.Plus)
      .map { parser =>
        val operator = parser.previousToken
        parser.unary.mapValue(Unary(operator, _))
      }
      .getOrElse(call)

  /**
   * {{{
   *   <call> ::= <primary> ("(" (<expression> | ("," <expression>)*)? ")" | "." <identifier>  | <index>)*
   * }}}
   */
  def call: ParseResult[Expr] = {
    // TODO: see if we can refactor this using the `sequence` utility from `Parser`
    def parenCall(callee: Expr, parser: Parser): ParseResult[Expr] = {
      def recurse(args: List[Expr], parser: Parser): ParseResult[List[Expr]] =
        parser
          .matchAny(TokenType.Comma)
          .fold(ParseResult.succeed(args.reverse, parser)) { parser =>
            parser.expression.flatMap { case Step(arg, parser) =>
              recurse(arg :: args, parser)
            }
          }

      val resultForArgs =
        if (!parser.check(TokenType.RightParen))
          parser.expression.flatMap(step => recurse(step.value :: Nil, step.next))
        else ParseResult.succeed(Nil, parser)

      resultForArgs.flatMap { case Step(_, next) =>
        next
          .consumeAfter(TokenType.RightParen, ")", "function call arguments")
          .flatMap(step =>
            resultForArgs.mapValue(callOrLambda(callee, _, step.value)).mapParser(_ => step.next)
          )
      }
    }

    primary.flatMap {
      // Recursively checks for function calls and property access
      def checkForCalls(step: Step[Expr]): ParseResult[Expr] = {
        def propAccess(expr: Expr, next: Parser) =
          next.consumeAfter(TokenType.Identifier, "property name", ".").mapValue(Get(expr, _))

        def indexAccess(expr: Expr, next: Parser) =
          next
            .consumeWith("key name", "after [") { case _: TokenType.Constant => true }
            .mapValue(IndexGet(expr, _))
            .flatMapParser(_.consumeAfter(TokenType.RightBracket, "]", "index access"))

        def checkForPropAccess = step.next
          .matchAny(TokenType.Dot)
          .fold(checkForIndexAccess)(propAccess(step.value, _).flatMap(checkForCalls))

        def checkForIndexAccess = step.next
          .matchAny(TokenType.LeftBracket)
          .fold(ParseResult.fromStep(step))(indexAccess(step.value, _).flatMap(checkForCalls))

        step.next
          .matchAny(TokenType.LeftParen)
          .fold(checkForPropAccess)(parenCall(step.value, _).flatMap(checkForCalls))
      }

      checkForCalls
    }
  }

  /**
   * Decides whether to construct a Call or a Lambda node. This is useful for partial application.
   * {{{
   *   let sum = lambda(x, y) { return x + y };
   *   let onePlus = sum(1, _);
   *
   *   println(onePlus(3));    // prints 4
   *   println(onePlus(5));    // prints 6
   * }}}
   * The partial application call above should desugar to the following code:
   * {{{let onePlus = lambda(y) { return sum(1, y); };}}}
   */
  private def callOrLambda(callee: Expr, args: List[Expr], paren: Token): Expr = {
    // concatenate all the param names to get a new unique one.
    // This will prevent name clashing
    lazy val uniqueBaseName = args.foldLeft("") {
      case (acc, Variable(Token(_, name, _))) => acc + name
      case (acc, _)                           => acc
    }

    val (params, newArgs) = args.zipWithIndex.foldLeft(List.empty[Token], List.empty[Expr]) {
      case ((params, args), (Variable(Token(_, Lexemes.Wildcard, line)), i)) =>
        val newParam = Token(TokenType.Identifier, uniqueBaseName + i, line)
        val newArg = Variable(newParam)
        (newParam :: params, newArg :: args)
      case ((params, args), (arg, i)) =>
        (params, arg :: args)
    }

    if (params.isEmpty) Call(callee, args, paren)
    else {
      val returnStatement =
        ReturnStmt(Token(TokenType.Return, Lexemes.Return, paren.line), Call(callee, newArgs.reverse, paren))
      Lambda(params.reverse, returnStatement :: Nil)
    }
  }

  /**
   * {{{
   *   <primary> ::= <constant> | "self" | <identifier> | <dictionary> | "(" <expression> ")""
   * }}}
   */
  def primary: ParseResult[Expr] =
    literal
      .orElse(matchAny(TokenType.Self).map(p => Step(Self(p.previousToken), p)))
      .orElse(matchAny(TokenType.Identifier).map(p => Step(Variable(p.previousToken), p)))
      .map(_.toParseResult)
      .getOrElse(
        dictionary.orElse(
          matchAny(TokenType.LeftParen)
            .fold[ParseResult[Expr]](
              ParseResult.fail(ParseError.expected(peek, "expression", "after ("), this)
            ) {
              _.expression.flatMap { case Step(expr, newParser) =>
                newParser.consumeAfter(TokenType.RightParen, ")", "expression").mapValue(_ => Grouping(expr))
              }
            }
        )
      )

  /**
   * {{{
   *    <dictionary> ::= "{" (<key-value> ("," <key-value>)*)? "}"
   *    <key-value>  ::= <constant> ":" <expression>
   * }}}
   */
  def dictionary: ParseResult[Expr] =
    sequence[(Token, Expr)](
      TokenType.LeftBrace,
      "{",
      TokenType.RightBrace,
      "}",
      "at the start of dictionary",
      "dictionary elements"
    )(_.matchWithAnyConstant.flatMap { next =>
      val key = next.previousToken
      next
        .consumeAfter(TokenType.Colon, ":", "dictionary key")
        .flatMap { case Step(_, next) =>
          next.expression.map(_.map((key, _)))
        }
        .fold[Option[Step[(Token, Expr)]]]((_, _) => None)(Some(_))
    }).mapValue(elements => Dictionary(elements.toMap))

  private[parsers] def literal: Option[Step[Literal]] =
    matchWithAnyConstant.map { next =>
      @nowarn
      val literal = next.previousToken.tokenType match {
        case TokenType.True          => Literal.True
        case TokenType.False         => Literal.False
        case TokenType.Str(string)   => Literal.Str(string)
        case TokenType.Number(value) => Literal.Number(value)
        case TokenType.None          => Literal.None
      }
      Step(literal, next)
    }

  private[parsers] def matchWithAnyConstant: Option[Parser] =
    matchAnyWith { case _: TokenType.Constant => true }

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
            val operator = parser.previousToken
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
