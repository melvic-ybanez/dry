package com.melvic.dry

import com.melvic.dry.result.Result.ResultCoAlg

package object resolver {
  type Scope = Map[String, Boolean]
  type Locals = Map[LocalExprKey, Int]
  type Context = (List[Scope], Locals)
  type Resolve = ResultCoAlg[Context]
}
