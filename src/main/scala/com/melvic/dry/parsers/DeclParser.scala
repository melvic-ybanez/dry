package com.melvic.dry.parsers

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Decl
import com.melvic.dry.ast.Decl.Let.{LetDecl, LetInit}
import com.melvic.dry.ast.Decl._
import com.melvic.dry.ast.Stmt.BlockStmt
import com.melvic.dry.lexer.Lexemes
import com.melvic.dry.parsers.Step._

import scala.util.chaining.scalaUtilChainingOps

//noinspection SpellCheckingInspection
private[parsers] trait DeclParser extends StmtParser { _: Parser =>

  /**
   * {{{<declaration> ::= <class> | <function> | <let> | <statement>}}}
   */
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

  /**
   * {{{<let> ::= "let" <identifier> ("=" <expression>)? ";"}}}
   */
  def letDecl: ParseResult[Let] = {
    def consumeSemicolon(parser: Parser): ParseResult[Token] =
      parser.consumeAfter(TokenType.Semicolon, ";", "let")

    consumeAfter(TokenType.Identifier, "identifier", "let").flatMap { case Step(name, parser) =>
      parser
        .matchAny(TokenType.Equal)
        .fold[ParseResult[Let]](consumeSemicolon(parser).as(LetDecl(name))) { parser =>
          parser.expression.flatMap { case Step(init, parser) =>
            consumeSemicolon(parser).as(LetInit(name, init))
          }
        }
    }
  }

  /**
   * {{{<function> ::= "def" <identifier> <params> <block>}}}
   */
  def defDecl(kind: String): ParseResult[Def] =
    for {
      name   <- consumeAfter(TokenType.Identifier, "identifier", s"'${Lexemes.Def}' keyword")
      params <- name.params
      body   <- params.functionBody(kind)
    } yield Step(Def(name.value, params.value, body.value.declarations), body.next)

  private[parsers] def functionBody(kind: String): ParseResult[BlockStmt] =
    for {
      leftBrace <- consumeAfter(TokenType.LeftBrace, "{", kind + " signature")
      body      <- leftBrace.block
    } yield body

  /**
   * {{{<class> ::= "class" <identifier> "{" <method>* "}"}}}
   */
  def classDecl: ParseResult[ClassDecl] =
    for {
      name       <- consumeAfter(TokenType.Identifier, "identifier", s"'${Lexemes.Class}' keyword")
      leftBrace  <- name.consumeAfter(TokenType.LeftBrace, "{", "class name")
      methods    <- leftBrace.methods
      rightBrace <- methods.consumeAfter(TokenType.RightBrace, "}", "class body")
    } yield Step(ClassDecl(name.value, methods.value), rightBrace.next)

  protected def methods: ParseResult[List[Def]] = {
    def recurse(parser: Parser, acc: List[Def]): ParseResult[List[Def]] =
      if (parser.check(TokenType.RightBrace) || parser.isAtEnd) ParseResult.succeed(acc.reverse, parser)
      else
        for {
          method   <- parser.consumeAfter(TokenType.Def, Lexemes.Def, "{ in class")
          function <- method.defDecl("method")
          result   <- recurse(function.next, function.value :: acc)
        } yield result

    recurse(this, Nil)
  }
}
