package com.melvic.dry.parsers

import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Decl
import com.melvic.dry.ast.Decl.{LetDecl, StmtDecl}

trait DeclParser extends StmtParser { _: Parser =>
  // TODO: Implement declaration
  def declaration: ParseResult[Decl] =
    matchAny(TokenType.Let) match {
      case None         => statement.mapValue(StmtDecl)
      case Some(parser) => parser.letDecl
    }

  def letDecl: ParseResult[LetDecl] = ???
}
