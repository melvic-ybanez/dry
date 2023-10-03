package com.melvic.dry.resolver

import com.melvic.dry.result.Result.ResultFrom

object ScopesFunction {
  type ScopesFunction = ResultFrom[List[Scope]]

  implicit class ScopesFunctionOps(function: ScopesFunction) {
    def ok: Resolve = context => function(context.scopes).map(scopes => context.copy(scopes = scopes))
  }
}
