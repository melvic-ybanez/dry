package com.melvic.dry.resolver

import com.melvic.dry.Token
import com.melvic.dry.aux.implicits._
import com.melvic.dry.resolver.ScopesFunction.ScopesFunction
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.ResultFrom
import com.melvic.dry.result.Result.implicits.ToResult

object Scopes {
  type Scopes = List[Scope]

  def empty: Scopes = List.empty[Scope]

  def start: ScopesFunction =
    scopes => (Map.empty[String, Boolean] :: scopes).ok

  def end: ScopesFunction = _.tail.ok

  def declare(name: Token): ScopesFunction =
    mapHead { scope =>
      if (scope.contains(name.lexeme)) Result.fail(ResolverError.variableAlreadyDefined(name))
      else (scope + (name.lexeme -> false)).ok
    }

  def define(name: Token): ScopesFunction =
    put(name.lexeme)

  def put(name: String): ScopesFunction =
    mapHeadOk(_ + (name -> true))

  def mapHead(update: ResultFrom[Scope]): ScopesFunction = {
    case Nil           => (Scopes.start >=> mapHead(update))(Nil)
    case scope :: rest => update(scope).map(_ :: rest)
  }

  def mapHeadOk(update: Scope => Scope): ScopesFunction =
    mapHead(update.andThen(_.ok))

  def resolveFromHead(update: Scope => Resolve): Resolve = {
    case context @ Context(Nil, _, _, _)        => context.ok
    case context @ Context(scope :: _, _, _, _) => update(scope)(context)
  }
}
