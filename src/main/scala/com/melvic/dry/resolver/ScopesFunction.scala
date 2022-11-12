package com.melvic.dry.resolver

import com.melvic.dry.result.Result.ResultCoAlg

object ScopesFunction {
  type ScopesFunction = ResultCoAlg[List[Scope]]

  implicit class ScopesFunctionOps(function: ScopesFunction) {
    def ok: Resolve = context => function(context.scopes).map(scopes => context.copy(scopes = scopes))
  }
}
