package com.melvic.dry.parsers

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Decl.StmtDecl
import com.melvic.dry.ast.Expr.Literal
import com.melvic.dry.ast.Stmt.IfStmt._
import com.melvic.dry.ast.Stmt.Loop.While
import com.melvic.dry.ast.Stmt.{BlockStmt, ExprStmt, Import, ReturnStmt}
import com.melvic.dry.ast.{Decl, Expr, Stmt}

private[parsers] trait StmtParser { _: Parser with DeclParser =>
  def statement: ParseResult[Stmt] =
    select(
      expressionStatement,
      TokenType.LeftBrace -> { _.block },
      TokenType.If        -> { _.ifStatement },
      TokenType.While     -> { _.whileStatement },
      TokenType.For       -> { _.forStatement },
      TokenType.Return    -> { _.returnStatement },
      TokenType.Import    -> { _.importStatement }
    )

  def expressionStatement: ParseResult[Stmt] =
    for {
      expr      <- expression
      semicolon <- expr.consume(TokenType.Semicolon, ";", "statement")
    } yield Step(ExprStmt(expr.value), semicolon.next)

  def block: ParseResult[BlockStmt] = {
    def recurse(result: ParseResult[List[Decl]]): ParseResult[List[Decl]] =
      result.flatMap { case Step(declarations, parser) =>
        if (parser.check(TokenType.RightBrace) || parser.isAtEnd) result.mapValue(_.reverse)
        else {
          parser.declaration.fold((moreErrors, newParser) =>
            recurse(result.combineErrors(moreErrors, newParser))
          ) { case Step(declaration, parser) =>
            recurse(ParseResult.succeed(declaration :: declarations, parser))
          }
        }
      }

    recurse(ParseResult.succeed(Nil, this)).flatMap { case Step(decls, parser) =>
      parser.consume(TokenType.RightBrace, "}", "block").mapValue(_ => BlockStmt(decls))
    }
  }

  def ifStatement: ParseResult[Stmt] =
    for {
      leftParen  <- consume(TokenType.LeftParen, "(", "if")
      cond       <- leftParen.expression
      rightParen <- cond.consume(TokenType.RightParen, ")", "if condition")
      thenBranch <- rightParen.statement
      ifStmt <- thenBranch
        .matchAny(TokenType.Else)
        .fold[ParseResult[Stmt]](
          ParseResult.succeed(IfThen(cond.value, thenBranch.value), thenBranch.next)
        ) {
          _.statement.mapValue { elseBranch =>
            IfThenElse(cond.value, thenBranch.value, elseBranch)
          }
        }
    } yield ifStmt

  def whileStatement: ParseResult[While] =
    for {
      leftParen  <- consume(TokenType.LeftParen, "(", "while")
      condition  <- leftParen.expression
      rightParen <- condition.consume(TokenType.RightParen, ")", "while condition")
      body       <- rightParen.statement
    } yield Step(While(condition.value, body.value), body.next)

  /**
   * A for-loop is just a syntactic sugar over the while-loop.
   */
  def forStatement: ParseResult[Stmt] = {
    val initializer = consume(TokenType.LeftParen, "(", "for").flatMap { case Step(_, next) =>
      next
        .matchAny(TokenType.Semicolon)
        .fold(
          next
            .matchAny(TokenType.Let)
            .fold[ParseResult[Decl]](next.expressionStatement)(_.letDecl)
        )(ParseResult.succeed(StmtDecl.fromExpr(Literal.None), _))
    }

    def clause(parser: Parser, delimiter: TokenType, consume: String, after: String) = {
      val clauseExpr =
        if (parser.check(delimiter)) ParseResult.succeed(Literal.None, parser)
        else parser.expression

      clauseExpr.flatMapParser(_.consume(delimiter, consume, after))
    }

    def condition(parser: Parser) = clause(parser, TokenType.Semicolon, ";", "for loop condition")

    def increment(parser: Parser) = clause(parser, TokenType.RightParen, ")", "for clauses")

    def whileLoop(init: Decl, cond: Expr, inc: Expr, body: Stmt) = {
      val bodyFromInc =
        if (inc == Literal.None) body
        else BlockStmt.append(body, ExprStmt(inc))
      val newCond = if (cond == Literal.None) Literal.True else cond
      val whileNode = While(newCond, bodyFromInc)
      init match {
        case StmtDecl(ExprStmt(Literal.None)) => whileNode
        case _                                => BlockStmt.fromDecls(init, whileNode)
      }
    }

    for {
      init <- initializer
      cond <- condition(init.next)
      inc  <- increment(cond.next)
      body <- inc.statement
    } yield Step(whileLoop(init.value, cond.value, inc.value, body.value), body.next)
  }

  def returnStatement: ParseResult[Stmt] = {
    val keyword = previous
    val expr =
      if (check(TokenType.Semicolon))
        consume(TokenType.Semicolon, ";", "return value").mapValue(_ => Literal.None)
      else expression.flatMapParser(_.consume(TokenType.Semicolon, ";", "return value"))
    expr.mapValue(ReturnStmt(keyword, _))
  }

  def importStatement: ParseResult[Stmt] = {
    def parseComponents(parser: Parser, components: List[Token]): ParseResult[List[Token]] =
      parser.matchAny(TokenType.Dot).fold(ParseResult.succeed(components.reverse, parser)) { next =>
        next.consume(TokenType.Identifier, "identifier", "import").flatMap { case Step(component, next) =>
          parseComponents(next, component :: components)
        }
      }

    for {
      firstComponent <- consume(TokenType.Identifier, "identifier", "import")
      allComponents  <- parseComponents(firstComponent.next, firstComponent.value :: Nil)
      semicolon      <- allComponents.consume(TokenType.Semicolon, ";", "import path")
    } yield Step(Import(allComponents.value), semicolon.next)
  }
}
