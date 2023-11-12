package com.melvic.dry.parsers

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Decl.StmtDecl
import com.melvic.dry.ast.Expr.{IndexGet, Literal, Variable}
import com.melvic.dry.ast.Stmt.IfStmt._
import com.melvic.dry.ast.Stmt.Loop.While
import com.melvic.dry.ast.Stmt._
import com.melvic.dry.ast.{Decl, Expr, Stmt}
import com.melvic.dry.aux.Nel
import com.melvic.dry.aux.Nel.{Many, One}
import com.melvic.dry.lexer.Lexemes

//noinspection ScalaWeakerAccess
private[parsers] trait StmtParser { _: Parser with DeclParser =>

  /**
   * {{{<statement> ::= <expr-stmt> | <block> | <if> | <while> | <for> | <return> | <import>}}}
   */
  def statement: ParseResult[Stmt] =
    select(
      expressionStatement,
      TokenType.LeftBrace -> { _.blockWithoutOpening },
      TokenType.If        -> { _.ifStatement },
      TokenType.While     -> { _.whileStatement },
      TokenType.For       -> { _.forStatement },
      TokenType.Return    -> { _.returnStatement },
      TokenType.Delete    -> { _.deleteStatement },
      TokenType.Try       -> { _.tryStatement },
      TokenType.Import    -> { _.importStatement }
    )

  /**
   * {{{<expr-stmt> ::= <expression> ";"}}}
   */
  def expressionStatement: ParseResult[Stmt] =
    for {
      expr      <- expression
      semicolon <- expr.consumeAfter(TokenType.Semicolon, ";", "statement")
    } yield Step(ExprStmt(expr.value), semicolon.next)

  def blockWithoutOpening: ParseResult[BlockStmt] = {
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
      parser.consumeAfter(TokenType.RightBrace, "}", "block").as(BlockStmt(decls))
    }
  }

  /**
   * {{{<block> ::= "{" <declaration>* "}"}}}
   */
  def block(after: String): ParseResult[BlockStmt] =
    for {
      leftBrace <- consumeAfter(TokenType.LeftBrace, "{", after)
      rest      <- leftBrace.blockWithoutOpening
    } yield rest

  /**
   * {{{<if> ::= "if" "(" <expression> ")" <statement> ("else" <statement>)?}}}
   */
  def ifStatement: ParseResult[Stmt] =
    for {
      leftParen  <- consumeAfter(TokenType.LeftParen, "(", "if")
      cond       <- leftParen.expression
      rightParen <- cond.consumeAfter(TokenType.RightParen, ")", "if condition")
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

  /**
   * {{{<while> ::= "while" "(" <expression> ")" <statement>}}}
   */
  def whileStatement: ParseResult[While] =
    for {
      leftParen  <- consumeAfter(TokenType.LeftParen, "(", "while")
      condition  <- leftParen.expression
      rightParen <- condition.consumeAfter(TokenType.RightParen, ")", "while condition")
      body       <- rightParen.statement
    } yield Step(While(condition.value, body.value), body.next)

  /**
   * A for-loop is just a syntactic sugar over the while-loop.
   * {{{
   *   <for> ::= "for" "(" (";" | <let> | <expr-stmt>)
   *    (<expression>? ";") <expression> ")" <statement>
   * }}}
   */
  def forStatement: ParseResult[Stmt] = {
    val initializer = consumeAfter(TokenType.LeftParen, "(", "for").flatMap { case Step(_, next) =>
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

      clauseExpr.flatMapParser(_.consumeAfter(delimiter, consume, after))
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

  /**
   * {{{<return> ::= "return" <expression>? ";"}}}
   */
  def returnStatement: ParseResult[Stmt] = {
    val keyword = previousToken
    val expr =
      if (check(TokenType.Semicolon))
        consumeAfter(TokenType.Semicolon, ";", "return value").as(Literal.None)
      else expression.flatMapParser(_.consumeAfter(TokenType.Semicolon, ";", "return value"))
    expr.mapValue(ReturnStmt(keyword, _))
  }

  /**
   * {{{<delete> ::= "del" <call><index> ";"}}}
   */
  // noinspection MutatorLikeMethodIsParameterless
  def deleteStatement: ParseResult[Stmt] =
    call
      .flatMap {
        case Step(IndexGet(obj, key, token), next) =>
          ParseResult.succeed(DeleteStmt(obj, key, token), next)
        case Step(expr, next) =>
          next
            .consumeAfter(TokenType.LeftBracket, "[", "call expression")
            .flatMap(next => next.indexAccess(DeleteStmt(expr, _, next.previousToken)))
      }
      .flatMapParser(_.consumeAfter(TokenType.Semicolon, ";", "]"))

  /**
   * {{{<try-catch> ::= "try" <block> ("catch" "(" (<identifier> ":") <identifier> ")" <block>)+}}}
   */
  def tryStatement: ParseResult[Stmt] = {
    def catchStmt(parser: Parser): ParseResult[CatchBlock] =
      for {
        leftParen <- parser.consumeAfter(TokenType.LeftParen, "(", Lexemes.Catch)
        exception <- leftParen
          .consumeAfter(TokenType.Identifier, "identifier", "(")
          .map(p => Step(Variable(p.next.previousToken), p))
        rightParen <- exception.consumeAfter(TokenType.RightParen, ")", "identifier")
        block      <- rightParen.block(")")
      } yield Step(CatchBlock(exception.value, block.value, leftParen.value), block.next)

    for {
      tryBlock   <- block(Lexemes.Try)
      catch_     <- tryBlock.consumeAfter(TokenType.Catch, Lexemes.Catch, "try block")
      catchBlock <- catchStmt(catch_)
      catchBlocks <- {
        def whileCatchBlock(parser: Parser, catchBlocks: Nel[CatchBlock]): ParseResult[Nel[CatchBlock]] =
          parser.matchAny(TokenType.Catch).fold(ParseResult.succeed(catchBlocks.reverse, parser)) { next =>
            catchStmt(next).flatMap { case Step(block, next2) =>
              whileCatchBlock(next2, block :: catchBlocks)
            }
          }

        whileCatchBlock(catchBlock.next, One(catchBlock.value))
      }
    } yield Step(TryCatch(tryBlock.value, Many(catchBlock.value, catchBlocks.value)), catchBlocks.next)
  }

  /**
   * {{{<import> ::= "import" <identifier>("."<identifier>)* ";"}}}
   */
  def importStatement: ParseResult[Stmt] = {
    def parseComponents(parser: Parser, components: List[Token]): ParseResult[List[Token]] =
      parser.matchAny(TokenType.Dot).fold(ParseResult.succeed(components.reverse, parser)) { next =>
        next.consumeAfter(TokenType.Identifier, "identifier", "import").flatMap {
          case Step(component, next) =>
            parseComponents(next, component :: components)
        }
      }

    for {
      firstComponent <- consumeAfter(TokenType.Identifier, "identifier", "import")
      allComponents  <- parseComponents(firstComponent.next, firstComponent.value :: Nil)
      semicolon      <- allComponents.consumeAfter(TokenType.Semicolon, ";", "import path")
    } yield Step(Import(allComponents.value), semicolon.next)
  }
}
