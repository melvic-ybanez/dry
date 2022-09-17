package com.melvic.dry.parsers

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Decl
import com.melvic.dry.ast.Decl._
import com.melvic.dry.parsers.Step._

import scala.util.chaining.scalaUtilChainingOps

private[parsers] trait DeclParser extends StmtParser { _: Parser =>
  def declaration: ParseResult[Decl] =
    matchAny(TokenType.Def)
      .map(_.defDecl)
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

  def defDecl: ParseResult[Def] =
    for {
      name      <- consume(TokenType.Identifier, "identifier", "def keyword")
      leftParen <- name.consume(TokenType.LeftParen, "(", "function name")
      params <-
        if (!leftParen.check(TokenType.RightParen)) {
          def recurse(params: List[Token], parser: Parser): ParseResult[List[Token]] =
            parser
              .matchAny(TokenType.Comma)
              .fold(ParseResult.succeed(params.reverse, parser))(
                _.consume(TokenType.Identifier, "parameter name", ",").mapValue(_ :: params)
              )

          leftParen.consume(TokenType.Identifier, "parameter name", "(").flatMap { case Step(param, parser) =>
            recurse(param :: Nil, parser).flatMapParser(_.consume(TokenType.RightParen, ")", "parameters"))
          }
        } else leftParen.consume(TokenType.RightParen, ")", "(").mapValue(_ => Nil)
      leftBrace <- params.consume(TokenType.LeftBrace, "{", "function signature")
      body      <- leftBrace.block
    } yield Step(Def(name.value, params.value, body.value.declarations), body.next)
}
