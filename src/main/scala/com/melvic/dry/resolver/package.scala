package com.melvic.dry

package object resolver {
  type Scope = Map[String, Boolean]
  type Locals = Map[LocalExprKey, Int]
  type Context = (List[Scope], Locals, FunctionType)
  type Resolve = Resolve.Resolve
}
