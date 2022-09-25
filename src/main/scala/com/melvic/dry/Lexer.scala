package com.melvic.dry

import com.melvic.dry.Lexer.enableEscapeSequences
import com.melvic.dry.Token.TokenType
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

  def updateTokens(f: List[Token] => List[Token]): Lexer =
    copy(tokens = f(tokens))

  def scan: Result[Lexer] = {
    val (char, lexer) = readAndAdvance
    char match {
      case '(' => lexer.addToken(TokenType.LeftParen).ok
      case ')' => lexer.addToken(TokenType.RightParen).ok
      case '{' => lexer.addToken(TokenType.LeftBrace).ok
      case '}' => lexer.addToken(TokenType.RightBrace).ok
      case ',' => lexer.addToken(TokenType.Comma).ok
      case '.' => lexer.addToken(TokenType.Dot).ok
      case '+' => lexer.addToken(TokenType.Plus).ok
      case '*' => lexer.addToken(TokenType.Star).ok
      case '%' => lexer.addToken(TokenType.Modulo).ok
      case '-' => lexer.addToken(TokenType.Minus).ok
      case ';' => lexer.addToken(TokenType.Semicolon).ok
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
          .fold(lexer.addTokenOrElse('=', TokenType.GreaterEqual, TokenType.Greater))(l =>
            l.matchChar('>').fold(l.addToken(TokenType.RightShift).ok)(_.addToken(TokenType.URightShift).ok)
          )
      case '/' =>
        lexer.matchChar('/').fold(lexer.addToken(TokenType.Slash).ok)(_.scanComment.ok)
      case ' ' | '\r' | '\t'     => lexer.ok
      case '\n'                  => lexer.nextLine.ok
      case '"'                   => lexer.scanString
      case c if Lexer.isDigit(c) => lexer.scanDigit.ok
      case c if Lexer.isAlpha(c) => lexer.scanIdentifier.ok
      case c                     => Result.fail(LexerError.invalidCharacter(line, c))
    }
  }

  def readAndAdvance: (Char, Lexer) =
    (source(current), advance)

  def peek: Char = peekN(1)

  def peekNext: Char = peekN(2)

  def peekN(n: Int): Char = {
    val index = current + n - 1
    if (index >= source.length) 0.toChar
    else source(index)
  }

  def advance: Lexer =
    copy(current = current + 1)

  def advanceWhile(predicate: Lexer => Boolean): Lexer = {
    @tailrec
    def loop(lexer: Lexer): Lexer =
      if (!predicate(lexer)) lexer
      else loop(lexer.advance)

    loop(this)
  }

  def nextLine: Lexer =
    copy(line = line + 1)

  def addToken(tokenType: TokenType): Lexer =
    updateTokens(_ :+ Token(tokenType, lexeme, line))

  def addTokenOrElse(char: Char, typeIfMatch: TokenType, typeIfNotMatch: TokenType): Result[Lexer] =
    matchChar(char).fold(addToken(typeIfNotMatch))(_.addToken(typeIfMatch)).ok

  def matchChar(expected: Char): Option[Lexer] =
    if (isAtEnd || source(current) != expected) None
    else Some(advance)

  def isAtEnd: Boolean = current >= source.length

  def prepareNext: Lexer = copy(start = current)

  def scanComment: Lexer =
    advanceWhile(lexer => lexer.peek != '\n' && !lexer.isAtEnd)

  def scanString: Result[Lexer] = {
    @tailrec
    def loop(lexer: Lexer): Lexer =
      if (lexer.peek == '"' || lexer.isAtEnd) lexer
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

  def scanDigit: Lexer = {
    val wholeNumber = advanceWhile(lexer => Lexer.isDigit(lexer.peek))
    val withFractional =
      if (wholeNumber.peek == '.' && Lexer.isDigit(wholeNumber.peekNext))
        wholeNumber.advance.advanceWhile(lexer => Lexer.isDigit(lexer.peek))
      else wholeNumber
    withFractional.addToken(TokenType.Number(withFractional.lexeme.toDouble))
  }

  def scanIdentifier: Lexer = {
    val lexer = advanceWhile(lexer => Lexer.isAlphanumeric(lexer.peek))
    val tokenType = Lexer.Keywords.getOrElse(lexer.lexeme, TokenType.Identifier)
    lexer.addToken(tokenType)
  }
}

object Lexer {
  val Keywords: Map[String, TokenType] = Map(
    "and"    -> TokenType.And,
    "or"     -> TokenType.Or,
    "class"  -> TokenType.Class,
    "if"     -> TokenType.If,
    "else"   -> TokenType.Else,
    "true"   -> TokenType.True,
    "false"  -> TokenType.False,
    "let"    -> TokenType.Let,
    "while"  -> TokenType.While,
    "for"    -> TokenType.For,
    "def"    -> TokenType.Def,
    "none"   -> TokenType.None,
    "return" -> TokenType.Return,
    "super"  -> TokenType.Super,
    "self"   -> TokenType.Self,
    "not"    -> TokenType.Not,
    "lambda" -> TokenType.Lambda
  )

  def scanTokens(source: String): Result[List[Token]] =
    Lexer.fromSource(source).map(_.tokens)

  def fromSource(source: String): Result[Lexer] = {
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

  def isDigit(char: Char): Boolean =
    Character.isDigit(char)

  def isAlpha(char: Char): Boolean =
    (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || char == '_'

  def isAlphanumeric(char: Char): Boolean =
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
