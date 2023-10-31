package com.melvic.dry.lexer

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.lexer.Lexer.enableEscapeSequences
import com.melvic.dry.result.Failure.LexerError
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.Result
import com.melvic.dry.result.Result.implicits.ToResult

import scala.annotation.tailrec

final case class Lexer(
    source: String,
    start: Int,
    current: Int,
    line: Int,
    tokens: List[Token]
) {
  def lexeme: String = source.substring(start, current)

  def currentChar: Char = source(current)

  def scanNext: Result[Lexer] = prepareNext.scan

  private def updateTokens(f: List[Token] => List[Token]): Lexer =
    copy(tokens = f(tokens))

  private def scan: Result[Lexer] = {
    val (char, lexer) = readAndAdvance
    char match {
      case '(' => lexer.addToken(TokenType.LeftParen).ok
      case ')' => lexer.addToken(TokenType.RightParen).ok
      case '{' => lexer.addToken(TokenType.LeftBrace).ok
      case '}' => lexer.addToken(TokenType.RightBrace).ok
      case '[' => lexer.addToken(TokenType.LeftBracket).ok
      case ']' => lexer.addToken(TokenType.RightBracket).ok
      case ',' => lexer.addToken(TokenType.Comma).ok
      case '.' => lexer.addToken(TokenType.Dot).ok
      case '+' => lexer.addToken(TokenType.Plus).ok
      case '*' => lexer.addToken(TokenType.Star).ok
      case '%' => lexer.addToken(TokenType.Modulo).ok
      case '-' => lexer.addToken(TokenType.Minus).ok
      case ';' => lexer.addToken(TokenType.Semicolon).ok
      case ':' => lexer.addToken(TokenType.Colon).ok
      case '!' => lexer.addTokenOrElse('=', TokenType.NotEqual, TokenType.Not)
      case '=' => lexer.addTokenOrElse('=', TokenType.EqualEqual, TokenType.Equal)
      case '&' => lexer.addToken(TokenType.BAnd).ok
      case '|' => lexer.addToken(TokenType.BOr).ok
      case '^' => lexer.addToken(TokenType.BXor).ok
      case '<' =>
        lexer
          .matchChar('<')
          .fold(lexer.addTokenOrElse('=', TokenType.LessEqual, TokenType.Less))(
            _.addToken(TokenType.LeftShift).ok
          )
      case '>' =>
        lexer
          .matchChar('>')
          .fold(lexer.addTokenOrElse('=', TokenType.GreaterEqual, TokenType.Greater))(
            _.addTokenOrElse('>', TokenType.URightShift, TokenType.RightShift)
          )
      case '/' =>
        lexer.matchChar('/').fold(lexer.addToken(TokenType.Slash).ok)(_.scanComment.ok)
      case ' ' | '\r' | '\t'     => lexer.ok
      case '\n'                  => lexer.nextLine.ok
      case '"'                   => lexer.scanString
      case c if Lexer.isDigit(c) => lexer.scanNumber.ok
      case c if Lexer.isAlpha(c) => lexer.scanIdentifier.ok
      case c                     => Result.fail(LexerError.invalidCharacter(line, c))
    }
  }

  private def readAndAdvance: (Char, Lexer) =
    (source(current), advance)

  def peek: Char = peekN(1)

  private def peekNext: Char = peekN(2)

  private def peekN(n: Int): Char = {
    val index = current + n - 1
    if (index >= source.length) 0.toChar
    else source(index)
  }

  def advance: Lexer =
    copy(current = current + 1)

  private def advanceWhile(predicate: Lexer => Boolean): Lexer = {
    @tailrec
    def loop(lexer: Lexer): Lexer =
      if (!predicate(lexer)) lexer
      else loop(lexer.advance)

    loop(this)
  }

  private def nextLine: Lexer =
    copy(line = line + 1)

  private def addToken(tokenType: TokenType): Lexer =
    updateTokens(_ :+ Token(tokenType, lexeme, line))

  private def addTokenOrElse(char: Char, typeIfMatch: TokenType, typeIfNotMatch: TokenType): Result[Lexer] =
    matchChar(char).fold(addToken(typeIfNotMatch))(_.addToken(typeIfMatch)).ok

  private def matchChar(expected: Char): Option[Lexer] =
    if (isAtEnd || source(current) != expected) None
    else Some(advance)

  def isAtEnd: Boolean = current >= source.length

  private def prepareNext: Lexer = copy(start = current)

  private def scanComment: Lexer =
    advanceWhile(lexer => lexer.peek != '\n' && !lexer.isAtEnd)

  /**
   * {{{<string> ::= '"'(.?"\n"?)*'"'}}}
   */
  private def scanString: Result[Lexer] = {
    @tailrec
    def loop(lexer: Lexer): Lexer =
      if ((lexer.peek == '"' || lexer.isAtEnd) && lexer.source(lexer.current - 1) != '\\') lexer
      else if (lexer.peek == '\n') loop(lexer.nextLine.advance)
      else loop(lexer.advance)

    val lexer = loop(this)
    if (lexer.isAtEnd) Result.fail(LexerError.unterminatedString(line))
    else {
      val newLexer = lexer.advance // remove the closing quotation mark
      val stringContent = newLexer.source.substring(newLexer.start + 1, newLexer.current - 1)
      newLexer.addToken(TokenType.Str(enableEscapeSequences(stringContent))).ok
    }
  }

  /**
   * {{{
   *  <number> ::= <sign>?<nat>("."<nat>)?
   *  <sign>   ::= "-" | "+"
   *  <nat>    ::= <digit><digit>*
   *  <digit>  ::= '0' ... '9'
   * }}}
   */
  private def scanNumber: Lexer = {
    val wholeNumber = advanceWhile(lexer => Lexer.isDigit(lexer.peek))
    val withFractional =
      if (wholeNumber.peek == '.' && Lexer.isDigit(wholeNumber.peekNext))
        wholeNumber.advance.advanceWhile(lexer => Lexer.isDigit(lexer.peek))
      else wholeNumber
    withFractional.addToken(TokenType.Number(withFractional.lexeme.toDouble))
  }

  /**
   * {{{<identifier> ::= <alpha>(<alpha>?<digit>?)*}}}
   */
  private def scanIdentifier: Lexer = {
    val lexer = advanceWhile(lexer => Lexer.isAlphanumeric(lexer.peek))
    val tokenType = Lexer.Keywords.getOrElse(lexer.lexeme, TokenType.Identifier)
    lexer.addToken(tokenType)
  }
}

object Lexer {
  val Keywords: Map[String, TokenType] = Map(
    Lexemes.And    -> TokenType.And,
    Lexemes.Or     -> TokenType.Or,
    Lexemes.Class  -> TokenType.Class,
    Lexemes.If     -> TokenType.If,
    Lexemes.Else   -> TokenType.Else,
    Lexemes.True   -> TokenType.True,
    Lexemes.False  -> TokenType.False,
    Lexemes.Let    -> TokenType.Let,
    Lexemes.While  -> TokenType.While,
    Lexemes.For    -> TokenType.For,
    Lexemes.Def    -> TokenType.Def,
    Lexemes.None   -> TokenType.None,
    Lexemes.Return -> TokenType.Return,
    Lexemes.Self   -> TokenType.Self,
    Lexemes.Not    -> TokenType.Not,
    Lexemes.Lambda -> TokenType.Lambda,
    Lexemes.Import -> TokenType.Import
  )

  def scanTokens(source: String): Result[List[Token]] =
    Lexer.fromSource(source).map(_.tokens)

  private def fromSource(source: String): Result[Lexer] = {
    @tailrec
    def loop(lexer: Lexer): Result[Lexer] =
      if (lexer.isAtEnd) Result.succeed(lexer)
      else
        lexer.scanNext match {
          case error @ Left(_) => error
          case Right(lexer)    => loop(lexer)
        }

    loop(Lexer(source, 0, 0, 1, Nil))
      .map(lexer => lexer.updateTokens(_ :+ Token(TokenType.Eof, "", lexer.line)))
  }

  private def isDigit(char: Char): Boolean =
    Character.isDigit(char)

  /**
   * {{{<alpha> ::= 'a' ... 'z' | 'A' ... 'Z' | '_'}}}
   */
  private def isAlpha(char: Char): Boolean =
    (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || char == '_'

  private def isAlphanumeric(char: Char): Boolean =
    isAlpha(char) || isDigit(char)

  private def enableEscapeSequences(value: String): String =
    value
      .replace("\\n", "\n")
      .replace("\\t", "\t")
      .replace("\\b", "\b")
      .replace("\\r", "\r")
      .replace("\\f", "\f")
      .replace("\\'", "\'")
      .replace("\\\"", "\"")
      .replace("\\\\", "\\")
}
