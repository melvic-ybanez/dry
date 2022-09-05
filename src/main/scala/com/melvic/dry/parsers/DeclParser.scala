package com.melvic.dry.parsers

import com.melvic.dry.ast.Decl
import com.melvic.dry.ast.Decl.StmtDecl
import com.melvic.dry.parsers.Parser.ParseResult

trait DeclParser extends StmtParser { _: Parser =>
  // TODO: Implement declaration
  def declaration: ParseResult[Decl] =
    statement.mapValue(StmtDecl)
}
