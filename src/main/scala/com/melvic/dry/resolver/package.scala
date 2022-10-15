package com.melvic.dry

import com.melvic.dry.ast.Expr
import com.melvic.dry.result.Result.ResultCoAlg

package object resolver {
  type Scope = Map[String, Boolean]
  type Locals = Map[Expr, Int]
  type Contexts = (List[Scope], Locals)
  type Resolve = ResultCoAlg[Contexts]
}
