package com.melvic.dry.parsers

import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Decl
import com.melvic.dry.ast.Decl.{LetDecl, StmtDecl}

import scala.util.chaining.scalaUtilChainingOps

trait DeclParser extends StmtParser { _: Parser =>
  def declaration: ParseResult[Decl] =
    (matchAny(TokenType.Let) match {
      case None         => statement.mapValue(StmtDecl)
      case Some(parser) => parser.letDecl
    }).pipe {
      case result @ ParseResult(Left(_), _) => result.mapParser(_.synchronize)
      case result => result
    }

  // TODO: Implement this function
  def letDecl: ParseResult[LetDecl] =
    ParseResult.failAll(Nil, this)
}
