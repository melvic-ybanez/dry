package com.melvic.dry.parsers

import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Stmt.{ExprStmt, PrintStmt}
import com.melvic.dry.ast.{Expr, Stmt}
import com.melvic.dry.parsers.Parser.ParseResult

private[parsers] trait StmtParser { _: Parser =>
  def statement: ParseResult[Stmt] =
    matchAny(TokenType.Print).fold(expressionStatement)(_.printStatement)

  def expressionStatement: ParseResult[Stmt] =
    expressionLikeStatement(ExprStmt)

  def printStatement: ParseResult[Stmt] =
    expressionLikeStatement(PrintStmt)

  private def expressionLikeStatement(f: Expr => Stmt): ParseResult[Stmt] =
    for {
      state1 <- expression
      state2 <- state1.parser.consume(TokenType.Semicolon, ";")
    } yield State(f(state1.value), state2.parser)
}
