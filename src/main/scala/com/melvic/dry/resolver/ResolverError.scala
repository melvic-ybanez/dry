package com.melvic.dry.resolver

import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.{Show, Token}
import com.melvic.dry.result.Failure
import com.melvic.dry.result.Failure.showLineAndMessage

sealed trait ResolverError extends Failure

object ResolverError {
  private final case class VariableAlreadyDefined(name: Token) extends ResolverError
  private final case class DeclaredButNotDefined(name: Token) extends ResolverError
  private final case class NotInsideAFunction(keyword: Token) extends ResolverError
  private final case class NotInsideAClass(keyword: Token) extends ResolverError
  private final case class ReturnFromInit(keyword: Token) extends ResolverError

  def variableAlreadyDefined(name: Token): ResolverError =
    VariableAlreadyDefined(name)

  def declaredButNotDefined(name: Token): ResolverError =
    DeclaredButNotDefined(name)

  def notInsideAFunction(keyword: Token): ResolverError =
    NotInsideAFunction(keyword)

  def notInsideAClass(keyword: Token): ResolverError =
    NotInsideAClass(keyword)

  def returnFromInit(keyword: Token): ResolverError =
    ReturnFromInit(keyword)

  def show: Show[ResolverError] = {
    case VariableAlreadyDefined(name) =>
      errorMsg(name, show"Variable $name is already defined in this scope")
    case DeclaredButNotDefined(name) => errorMsg(name, show"'$name' is declared but not yet defined")
    case NotInsideAFunction(keyword) => errorMsg(keyword, show"'$keyword' is not inside a function")
    case NotInsideAClass(keyword)    => errorMsg(keyword, show"'$keyword' is not inside a class")
    case ReturnFromInit(keyword)     => errorMsg(keyword, show"'return' is not allowed inside a constructor")
  }

  private def errorMsg(token: Token, message: String): String =
    showLineAndMessage(token.line, s"Resolver Error: $message")
}
