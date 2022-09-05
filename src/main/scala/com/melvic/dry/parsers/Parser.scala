package com.melvic.dry.parsers

import com.melvic.dry.Error.ParseError
import com.melvic.dry.Result.Result
import com.melvic.dry.Result.impilcits.ToResult
import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Decl
import com.melvic.dry.parsers.Parser.{ParseResult, State}
import com.melvic.dry.{Result, Token}

import scala.util.chaining._

final case class Parser(tokens: List[Token], current: Int) extends ExprParser with DeclParser {
  def parse: Result[List[Decl]] = {
    def recurse(parser: Parser, statements: List[Decl]): Result[List[Decl]] =
      if (parser.isAtEnd) statements.reverse.ok
      else
        parser.declaration.flatMap { case (stmt, newParser) =>
          recurse(newParser, stmt :: statements)
        }

    recurse(this, Nil)
  }

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

  def advance: State[Token] =
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
  type State[A]       = (A, Parser)
  type ParseResult[A] = Result[State[A]]

  def fromTokens(tokens: List[Token]): Parser =
    Parser(tokens, 0)

  implicit class StateOps[A](state: State[A]) {
    def value: A       = state._1
    def parser: Parser = state._2

    def map[B](f: A => B): State[B] =
      (f(value), parser)
  }

  implicit class ParseResultOps[A](result: ParseResult[A]) {
    def mapValue[B](f: A => B): ParseResult[B] =
      result.map(_.map(f))
  }
}
