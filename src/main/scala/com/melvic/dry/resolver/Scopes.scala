package com.melvic.dry.resolver

import com.melvic.dry.Token
import com.melvic.dry.resolver.ScopesFunction.ScopesFunction
import com.melvic.dry.result.Result.ResultCoAlg
import com.melvic.dry.result.Result.implicits.ToResult

object Scopes {
  def start: ScopesFunction =
    scopes => (Map.empty[String, Boolean] :: scopes).ok

  def end: ScopesFunction = _.tail.ok

  def declare(name: Token): ScopesFunction =
    mapHeadOk(_ + (name.lexeme -> false))

  def define(name: Token): ScopesFunction =
    mapHeadOk(_ + (name.lexeme -> true))

  def mapHead(update: ResultCoAlg[Scope]): ScopesFunction = {
    case Nil           => Nil.ok
    case scope :: rest => update(scope).map(_ :: rest)
  }

  def mapHeadOk(update: Scope => Scope): ScopesFunction =
    mapHead(update.andThen(_.ok))

  def resolveFromHead(update: Scope => Resolve): Resolve = {
    case env @ (Nil, _)                => env.ok
    case (scopes @ scope :: _, locals) => update(scope)(scopes, locals)
  }
}
