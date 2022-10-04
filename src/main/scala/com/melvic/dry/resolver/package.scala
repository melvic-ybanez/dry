package com.melvic.dry

import com.melvic.dry.result.{Failure, Result}
import com.melvic.dry.result.Result.Result

package object resolver {
  type Scope = Map[String, Boolean]
  type Scopes = Scopes.Scopes
  type ScopesUpdateF[F[_]] = F[Scope] => Result[F[Scope]]
  type ScopesFunction = ScopesUpdateF[List]
  type ScopeUpdate = ScopesUpdateF[Id]
  type Resolve[A] = A => ScopesFunction

  object ScopesFunction {
    def fail(failure: Failure): ScopesFunction =
      _ => Result.fail(failure)
  }
}
