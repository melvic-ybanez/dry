package com.melvic.dry.resolver

import com.melvic.dry.Token
import com.melvic.dry.result.Result.implicits.ToResult

object Scopes {
  type Scopes = List[Scope]

  def start: ScopesFunction =
    scopes => (Map.empty[String, Boolean] :: scopes).ok

  def endScope: ScopesFunction = _.tail.ok

  def declare(name: Token): ScopesFunction =
    mapHeadOk(_ + (name.lexeme -> false))

  def define(name: Token): ScopesFunction =
    mapHeadOk(_ + (name.lexeme -> true))

  def mapHead(update: ScopeUpdate): ScopesFunction = {
    case Nil           => Nil.ok
    case scope :: rest => update(scope).map(_ :: rest)
  }

  def mapHeadOk(update: Scope => Scope): ScopesFunction =
    mapHead(update.andThen(_.ok))

  def flatMapHead(update: Scope => ScopesFunction): ScopesFunction = {
    case Nil => Nil.ok
    case scopes@scope :: _ => update(scope)(scopes)
  }
}
