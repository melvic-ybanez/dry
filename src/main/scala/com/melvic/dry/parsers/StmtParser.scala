package com.melvic.dry.parsers

import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Stmt.IfStmt._
import com.melvic.dry.ast.Stmt.Loop.While
import com.melvic.dry.ast.Stmt.{BlockStmt, ExprStmt, PrintStmt}
import com.melvic.dry.ast.{Decl, Expr, Stmt}

private[parsers] trait StmtParser { _: Parser with DeclParser =>
  def statement: ParseResult[Stmt] =
    select(
      expressionStatement,
      TokenType.Print     -> { _.printStatement },
      TokenType.LeftBrace -> { _.block },
      TokenType.If        -> { _.ifStatement },
      TokenType.While     -> { _.whileStatement }
    )

  def expressionStatement: ParseResult[Stmt] =
    expressionLikeStatement(ExprStmt)

  def printStatement: ParseResult[Stmt] =
    expressionLikeStatement(PrintStmt)

  def block: ParseResult[BlockStmt] = {
    def recurse(result: ParseResult[List[Decl]]): ParseResult[List[Decl]] =
      result.flatMap { case State(declarations, parser) =>
        if (parser.check(TokenType.RightBrace) || parser.isAtEnd) result.mapValue(_.reverse)
        else
          parser.declaration.flatMap { case State(declaration, parser) =>
            recurse(ParseResult.succeed(declaration :: declarations, parser))
          }
      }

    recurse(ParseResult.succeed(Nil, this)).flatMap { case State(decls, parser) =>
      parser.consume(TokenType.RightBrace, "}", "block").mapValue(_ => BlockStmt(decls))
    }
  }

  def ifStatement: ParseResult[Stmt] =
    for {
      state      <- consume(TokenType.LeftParen, "(", "if")
      cond       <- state.expression
      body       <- cond.consume(TokenType.RightParen, ")", "if condition")
      thenBranch <- body.statement
      ifStmt <- thenBranch
        .matchAny(TokenType.Else)
        .fold[ParseResult[Stmt]](
          ParseResult.succeed(IfThen(cond.value, thenBranch.value), thenBranch.parser)
        ) {
          _.statement.mapValue { elseBranch =>
            IfThenElse(cond.value, thenBranch.value, elseBranch)
          }
        }
    } yield ifStmt

  def whileStatement: ParseResult[Stmt] =
    for {
      leftParen  <- consume(TokenType.LeftParen, "(", "while")
      condition  <- leftParen.expression
      rightParen <- condition.consume(TokenType.RightParen, ")", "while condition")
      body       <- rightParen.statement
    } yield State(While(condition.value, body.value), body.parser)

  private def expressionLikeStatement(f: Expr => Stmt): ParseResult[Stmt] =
    for {
      expr      <- expression
      semicolon <- expr.consume(TokenType.Semicolon, ";", "statement")
    } yield State(f(expr.value), semicolon.parser)
}
