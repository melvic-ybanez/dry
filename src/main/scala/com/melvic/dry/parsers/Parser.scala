package com.melvic.dry.parsers

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Decl
import com.melvic.dry.result.Failure.ParseError

import scala.annotation.tailrec
import scala.util.chaining._

final case class Parser(tokens: List[Token], current: Int) extends ExprParser with DeclParser {
  def parse: ParseResult[List[Decl]] = {
    def recurse(result: ParseResult[List[Decl]]): ParseResult[List[Decl]] =
      if (result.parser.isAtEnd) result.mapValue(_.reverse)
      else
        result.parser.declaration.fold((moreErrors, newParser) =>
          recurse(result.combineErrors(moreErrors, newParser))
        ) { case Step(stmt, newParser) =>
          recurse(result.mapValue(stmt :: _).mapParser(_ => newParser))
        }

    recurse(ParseResult.succeed(Nil, this))
  }

  def matchAny(tokenTypes: TokenType*): Option[Parser] =
    matchAnyWith(tokenType => tokenTypes.contains(tokenType))

  def matchAnyWith(predicate: PartialFunction[TokenType, Boolean]): Option[Parser] =
    checkWith(predicate).pipe {
      case true  => Some(advance.next)
      case false => None
    }

  private def checkWith(predicate: PartialFunction[TokenType, Boolean]): Boolean =
    if (isAtEnd) false
    else predicate.applyOrElse(peek.tokenType, (_: TokenType) => false)

  def check(tokenType: TokenType): Boolean =
    checkWith(_ == tokenType)

  /**
   * A switch-case version of [[matchAny]]. You provide a list of choices and a default parse result. Each
   * choice maps a token-type to a parse function. The first choice with the first component that satisfies
   * [[matchAny]] will invoke its corresponding parse function.
   */
  def select[A](default: ParseResult[A], choice: (TokenType, Parser => ParseResult[A])*): ParseResult[A] = {
    def recurse(choices: List[(TokenType, Parser => ParseResult[A])]): ParseResult[A] =
      choices match {
        case Nil                 => default
        case (choice, f) :: rest => matchAny(choice).fold(recurse(rest))(f)
      }

    recurse(choice.toList)
  }

  def advance: Step[Token] =
    if (isAtEnd) Step(previousToken, this)
    else {
      val parser = copy(current = current + 1)
      Step(parser.previousToken, parser)
    }

  def consume(tokenType: TokenType, expected: String, after: String): ParseResult[Token] =
    if (check(tokenType)) advance.toParseResult
    else ParseResult.fail(ParseError.expected(peek, expected, after), this)

  def isAtEnd: Boolean =
    peek.tokenType == TokenType.Eof

  def peek: Token =
    tokens(current)

  def previousToken: Token =
    tokens(current - 1)

  /**
   * Synchronizes the parser by skipping all the lexemes that are assumed to be affected by an error.
   */
  def synchronize: Parser =
    advance.pipe { case Step(_, parser) =>
      @tailrec
      def recurse(parser: Parser): Parser =
        if (parser.isAtEnd) parser
        else if (parser.previousToken.tokenType == TokenType.Semicolon) parser
        else
          parser.peek.tokenType match {
            case TokenType.Class | TokenType.Def | TokenType.If | TokenType.For | TokenType.While |
                TokenType.Let | TokenType.Return =>
              parser
            case _ => recurse(parser.advance.next)
          }

      recurse(parser)
    }

  /**
   * {{{<params> ::= "(" <identifier>* ")"}}}
   */
  def params: ParseResult[List[Token]] =
    for {
      afterLeftParen <- consume(TokenType.LeftParen, "(", "function name")
      params <- {
        def parseWhileNextIsComma(params: List[Token], parser: Parser): ParseResult[List[Token]] =
          parser
            .matchAny(TokenType.Identifier)
            .fold(ParseResult.succeed(params, parser)) { next =>
              val newParams = next.previousToken :: params
              next
                .matchAny(TokenType.Comma)
                .fold(ParseResult.succeed(newParams, next))(parseWhileNextIsComma(newParams, _))
            }

        parseWhileNextIsComma(Nil, afterLeftParen)
          .mapValue(_.reverse)
          .flatMapParser(_.consume(TokenType.RightParen, ")", "parameters"))
      }
    } yield params
}

object Parser {
  def fromTokens(tokens: List[Token]): Parser =
    Parser(tokens, 0)
}
