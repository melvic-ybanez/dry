package com.melvic.dry.parsers

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Decl
import com.melvic.dry.result.Failure.ParseError

import scala.annotation.tailrec
import scala.util.chaining._

final case class Parser(tokens: List[Token], current: Int) extends ExprParser with DeclParser {
  def parse: ParseResult[List[Decl]] = {
    def recurse(parser: Parser, statements: List[Decl]): ParseResult[List[Decl]] =
      if (parser.isAtEnd) ParseResult.succeed(statements.reverse, parser)
      else
        parser.declaration.flatMap { case State(stmt, newParser) =>
          recurse(newParser, stmt :: statements)
        }

    recurse(this, Nil)
  }

  def matchAny(tokenTypes: TokenType*): Option[Parser] =
    matchAnyWith(tokenType => tokenTypes.contains(tokenType))

  def matchAnyWith(predicate: PartialFunction[TokenType, Boolean]): Option[Parser] =
    checkWith(predicate).pipe {
      case true  => Some(advance.parser)
      case false => None
    }

  def checkWith(predicate: PartialFunction[TokenType, Boolean]): Boolean =
    if (isAtEnd) false
    else predicate.applyOrElse(peek.tokenType, (_: TokenType) => false)

  def check(tokenType: TokenType): Boolean =
    checkWith(_ == tokenType)

  def advance: State[Token] =
    if (isAtEnd) State(previous, this)
    else {
      val parser = copy(current = current + 1)
      State(parser.previous, parser)
    }

  def consume(tokenType: TokenType, expected: String): ParseResult[Token] =
    if (check(tokenType)) advance.toParseResult
    else ParseResult.fail(ParseError.expected(peek, expected), this)

  def isAtEnd: Boolean =
    peek.tokenType == TokenType.Eof

  def peek: Token =
    tokens(current)

  def previous: Token =
    tokens(current - 1)

  def synchronize: Parser =
    advance.pipe { case State(_, parser) =>
      @tailrec
      def recurse(parser: Parser): Parser =
        if (parser.isAtEnd) parser
        else if (parser.previous.tokenType == TokenType.Semicolon) parser
        else
          parser.peek.tokenType match {
            case TokenType.Class | TokenType.Def | TokenType.If | TokenType.For | TokenType.While |
                TokenType.Let | TokenType.Return | TokenType.Print =>
              parser
            case _ => recurse(parser.advance.parser)
          }

      recurse(parser)
    }
}

object Parser {
  def fromTokens(tokens: List[Token]): Parser =
    Parser(tokens, 0)
}
