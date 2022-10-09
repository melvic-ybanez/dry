package com.melvic.dry

import com.melvic.dry.ast.Expr
import com.melvic.dry.result.Result.ResultCoAlg

package object resolver {
  type Scope = Map[String, Boolean]
  type Env = (List[Scope], Map[Expr, Int])
  type Resolve = ResultCoAlg[Env]
}
