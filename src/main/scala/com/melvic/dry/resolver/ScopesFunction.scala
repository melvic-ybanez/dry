package com.melvic.dry.resolver

import com.melvic.dry.result.Result.ResultCoAlg
import com.melvic.dry.result.{Failure, Result}

object ScopesFunction {
  type ScopesFunction = ResultCoAlg[List[Scope]]

  def fail(failure: Failure): Resolve =
    _ => Result.fail(failure)

  implicit class ScopesFunctionOps(function: ScopesFunction) {
    def ok: Resolve = { case (scopes, locals) => function(scopes).map((_, locals)) }
  }
}
