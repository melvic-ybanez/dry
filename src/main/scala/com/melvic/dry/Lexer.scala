package com.melvic.dry

import com.melvic.dry.Token.TokenType

import scala.annotation.tailrec

final case class Lexer(
    source: String,
    start: Int,
    current: Int,
    line: Int,
    tokens: Vector[Token]
) {
  def lexeme: String = source.substring(start, current)

  def currentChar: Char = source(current)

  def scanNext: Result[Lexer] = prepareNext.scan

  def scan: Result[Lexer] = {
    val (char, lexer) = readAndAdvance
    char match {
      case '(' => lexer.addTokenSuccess(TokenType.LeftParen)
      case ')' => lexer.addTokenSuccess(TokenType.RightParen)
      case '{' => lexer.addTokenSuccess(TokenType.LeftBrace)
      case '}' => lexer.addTokenSuccess(TokenType.RightBrace)
      case ',' => lexer.addTokenSuccess(TokenType.Comma)
      case '.' => lexer.addTokenSuccess(TokenType.Dot)
      case '+' => lexer.addTokenSuccess(TokenType.Plus)
      case '*' => lexer.addTokenSuccess(TokenType.Star)
      case '-' => lexer.addTokenSuccess(TokenType.Minus)
      case ';' => lexer.addTokenSuccess(TokenType.Semicolon)
      case '!' => lexer.addTokenOrElse('=', TokenType.NotEqual, TokenType.Not)
      case '=' => lexer.addTokenOrElse('=', TokenType.EqualEqual, TokenType.Equal)
      case '<' => lexer.addTokenOrElse('=', TokenType.LessEqual, TokenType.Less)
      case '>' => lexer.addTokenOrElse('=', TokenType.GreaterEqual, TokenType.Greater)
      case '/' =>
        lexer.matchChar('/').fold(lexer.addTokenSuccess(TokenType.Slash))(_.scanComment.success)
      case ' ' | '\r' | '\t'     => lexer.success
      case '\n'                  => lexer.nextLine.success
      case '"'                   => lexer.scanString
      case c if Lexer.isDigit(c) => lexer.scanDigit.success
      case c if Lexer.isAlpha(c) => lexer.scanIdentifier.success
      case c                     => Result.error(Error.invalidCharacter(line, c))
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
    copy(tokens = tokens :+ Token(tokenType, lexeme, line))

  def addTokenSuccess(tokenType: TokenType): Result[Lexer] =
    addToken(tokenType).success

  def addTokenOrElse(char: Char, typeIfMatch: TokenType, typeIfNotMatch: TokenType): Result[Lexer] =
    matchChar(char).fold(addToken(typeIfNotMatch))(_.addToken(typeIfMatch)).success

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
    if (lexer.isAtEnd) Result.error(Error.unterminatedString(line))
    else {
      val newLexer = lexer.advance // remove the closing quotation mark
      val stringContent = newLexer.source.substring(newLexer.start + 1, newLexer.current - 1)
      newLexer.addToken(TokenType.Str(stringContent)).success
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
      .addToken(TokenType.Identifier)
    val tokenType = Lexer.Keywords.getOrElse(lexer.lexeme, TokenType.Identifier)
    lexer.addToken(tokenType)
  }

  def success: Result[Lexer] =
    Result.success(this)
}

object Lexer {
  val Keywords: Map[String, TokenType] = Map(
    "and" -> TokenType.And,
    "or" -> TokenType.Or,
    "class" -> TokenType.Class,
    "if" -> TokenType.If,
    "else" -> TokenType.Else,
    "true" -> TokenType.True,
    "false" -> TokenType.False,
    "let" -> TokenType.Let,
    "while" -> TokenType.While,
    "for" -> TokenType.For,
    "def" -> TokenType.Def,
    "none" -> TokenType.None,
    "print" -> TokenType.Print,
    "return" -> TokenType.Return,
    "super" -> TokenType.Super,
    "self" -> TokenType.Self
  )

  def scanTokens(source: String): Result[Vector[Token]] =
    Lexer.fromSource(source).map(_.tokens)

  def fromSource(source: String): Result[Lexer] = {
    @tailrec
    def loop(lexer: Lexer): Result[Lexer] =
      if (lexer.isAtEnd) Result.success(lexer)
      else
        lexer.scanNext match {
          case error @ Left(_) => error
          case Right(lexer)    => loop(lexer)
        }

    loop(Lexer(source, 0, 0, 1, Vector.empty))
  }

  def isDigit(char: Char): Boolean =
    Character.isDigit(char)

  def isAlpha(char: Char): Boolean =
    (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || char == '_'

  def isAlphanumeric(char: Char): Boolean =
    isAlpha(char) || isDigit(char)
}
