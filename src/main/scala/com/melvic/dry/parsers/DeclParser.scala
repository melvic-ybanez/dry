package com.melvic.dry.parsers

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Decl
import com.melvic.dry.ast.Decl.{Let, LetDecl, LetInit, StmtDecl}

import scala.util.chaining.scalaUtilChainingOps

private[parsers] trait DeclParser extends StmtParser { _: Parser =>
  def declaration: ParseResult[Decl] =
    (matchAny(TokenType.Let) match {
      case None         => statement.mapValue(StmtDecl)
      case Some(parser) => parser.letDecl
    }).pipe {
      case result @ ParseResult(Left(_), _) => result.mapParser(_.synchronize)
      case result                           => result
    }

  def letDecl: ParseResult[Let] = {
    def consumeSemicolon(parser: Parser): ParseResult[Token] =
      parser.consume(TokenType.Semicolon, ";", "let")

    consume(TokenType.Identifier, "identifier", "let").flatMap { case State(name, parser) =>
      parser.matchAny(TokenType.Equal)
        .fold[ParseResult[Let]](consumeSemicolon(parser).mapValue(_ => LetDecl(name))) { parser =>
          parser.expression.flatMap { case State(init, parser) =>
            consumeSemicolon(parser).mapValue(_ => LetInit(name, init))
          }
        }
    }
  }
}
