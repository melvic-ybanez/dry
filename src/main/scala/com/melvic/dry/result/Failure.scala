package com.melvic.dry.result

import com.melvic.dry.Show
import com.melvic.dry.interpreter.errors.{RaisedError, RuntimeError}
import com.melvic.dry.lexer.LexerError
import com.melvic.dry.parsers.ParseError
import com.melvic.dry.resolver.ResolverError
import com.melvic.dry.result.Result.Result

trait Failure

object Failure {
  final case class Line(line: Int, where: String, message: String) extends Failure

  def line(line: Int, message: String): Failure =
    Line(line, "", message)

  def showFullLine(line: Int, where: String, message: String): String =
    s"${showLine(line)} Error $where: $message"

  def showLineAndMessage(line: Int, message: String): String =
    showFullLine(line, "", message)

  def show: Show[Failure] = {
    case Line(line, where, message)     => showFullLine(line, where, message)
    case lexerError: LexerError         => LexerError.show(lexerError)
    case parseError: ParseError         => ParseError.show(parseError)
    case runtimeError: RuntimeError     => RuntimeError.show(runtimeError)
    case resolutionError: ResolverError => ResolverError.show(resolutionError)
    case exception: RaisedError         => RaisedError.show(exception)
  }

  implicit class FailureOps(failure: Failure) {
    def fail[A]: Result[A] = Result.fail(failure)
  }

  def showLine(line: Int): String =
    s"[line $line]"
}
