package com.melvic.dry.parsers

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Decl
import com.melvic.dry.ast.Decl.Let.{LetDecl, LetInit}
import com.melvic.dry.ast.Decl._
import com.melvic.dry.ast.Stmt.BlockStmt
import com.melvic.dry.parsers.Step._

import scala.util.chaining.scalaUtilChainingOps

private[parsers] trait DeclParser extends StmtParser { _: Parser =>
  def declaration: ParseResult[Decl] =
    matchAny(TokenType.Class)
      .map(_.classDecl)
      .orElse(matchAny(TokenType.Def).map(_.defDecl("function")))
      .orElse(matchAny(TokenType.Let).map(_.letDecl))
      .getOrElse(statement.mapValue(StmtDecl(_)))
      .pipe {
        case result @ ParseResult(Left(_), _) => result.mapParser(_.synchronize)
        case result                           => result
      }

  def letDecl: ParseResult[Let] = {
    def consumeSemicolon(parser: Parser): ParseResult[Token] =
      parser.consume(TokenType.Semicolon, ";", "let")

    consume(TokenType.Identifier, "identifier", "let").flatMap { case Step(name, parser) =>
      parser
        .matchAny(TokenType.Equal)
        .fold[ParseResult[Let]](consumeSemicolon(parser).mapValue(_ => LetDecl(name))) { parser =>
          parser.expression.flatMap { case Step(init, parser) =>
            consumeSemicolon(parser).mapValue(_ => LetInit(name, init))
          }
        }
    }
  }

  def defDecl(kind: String): ParseResult[Def] =
    for {
      name   <- consume(TokenType.Identifier, "identifier", "'def' keyword")
      params <- name.params
      body   <- params.functionBody(kind)
    } yield Step(Def(name.value, params.value, body.value.declarations), body.next)

  private[parsers] def functionBody(kind: String): ParseResult[BlockStmt] =
    for {
      leftBrace <- consume(TokenType.LeftBrace, "{", kind + " signature")
      body      <- leftBrace.block
    } yield body

  def classDecl: ParseResult[ClassDecl] =
    for {
      name       <- consume(TokenType.Identifier, "identifier", "'class' keyword")
      leftBrace  <- name.consume(TokenType.LeftBrace, "{", "class name")
      methods    <- leftBrace.methods
      rightBrace <- methods.consume(TokenType.RightBrace, "}", "class body")
    } yield Step(ClassDecl(name.value, methods.value), rightBrace.next)

  protected def methods: ParseResult[List[Def]] = {
    def recurse(parser: Parser, acc: List[Def]): ParseResult[List[Def]] =
      if (parser.check(TokenType.RightBrace) || parser.isAtEnd) ParseResult.succeed(acc.reverse, parser)
      else
        parser.defDecl("method").flatMap { case Step(function, next) =>
          recurse(next, function :: acc)
        }

    recurse(this, Nil)
  }
}
