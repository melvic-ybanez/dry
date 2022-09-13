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

  def advance: State[Token] =
    if (isAtEnd) State(previous, this)
    else {
      val parser = copy(current = current + 1)
      State(parser.previous, parser)
    }

  def consume(tokenType: TokenType, expected: String, after: String): ParseResult[Token] =
    if (check(tokenType)) advance.toParseResult
    else ParseResult.fail(ParseError.expected(peek, expected, after), this)

  def isAtEnd: Boolean =
    peek.tokenType == TokenType.Eof

  def peek: Token =
    tokens(current)

  def previous: Token =
    tokens(current - 1)

  /**
   * Synchronizes the parser by skipping all the lexemes that are assumed to be affected by an error.
   */
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
