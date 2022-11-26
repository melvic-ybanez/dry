package com.melvic.dry

import com.melvic.dry.Token.TokenType

final case class Token(tokenType: TokenType, lexeme: String, line: Int)

object Token {
  sealed trait TokenType { self =>
    def apply(lexeme: String, line: Int): Token =
      Token(this, lexeme, line)

    def unapply(token: Token): Option[(String, Int)] =
      if (token.tokenType == this) Some(token.lexeme, token.line)
      else None
  }

  object TokenType extends Arithmetic with Bitwise with Comparison with Literals with Keywords with Others {
    case object Eof extends TokenType
  }

  trait Arithmetic {
    case object Minus extends TokenType
    case object Plus extends TokenType
    case object Slash extends TokenType
    case object Star extends TokenType
    case object Modulo extends TokenType
  }

  trait Bitwise {
    case object BAnd extends TokenType
    case object BOr extends TokenType
    case object BXor extends TokenType
    case object LeftShift extends TokenType
    case object RightShift extends TokenType
    case object URightShift extends TokenType
  }

  trait Comparison {
    case object NotEqual extends TokenType
    case object Equal extends TokenType
    case object EqualEqual extends TokenType
    case object Greater extends TokenType
    case object GreaterEqual extends TokenType
    case object Less extends TokenType
    case object LessEqual extends TokenType
  }

  trait Others {
    case object LeftParen extends TokenType
    case object RightParen extends TokenType
    case object LeftBrace extends TokenType
    case object RightBrace extends TokenType
    case object Comma extends TokenType
    case object Dot extends TokenType
    case object Semicolon extends TokenType
  }

  trait Literals {
    case object Identifier extends TokenType
    final case class Str(value: String) extends TokenType
    final case class Number(value: Double) extends TokenType
  }

  trait Keywords {
    case object And extends TokenType
    case object Not extends TokenType
    case object Or extends TokenType
    case object Class extends TokenType
    case object Else extends TokenType
    case object False extends TokenType
    case object Def extends TokenType
    case object For extends TokenType
    case object If extends TokenType
    case object None extends TokenType
    case object Return extends TokenType
    case object Self extends TokenType
    case object True extends TokenType
    case object Let extends TokenType
    case object While extends TokenType
    case object Lambda extends TokenType
  }

  def show: Show[Token] = _.lexeme
}
