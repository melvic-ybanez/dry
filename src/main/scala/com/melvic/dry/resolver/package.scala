package com.melvic.dry

package object resolver {
  type Scope = Map[String, Boolean]
  type Locals = Map[LocalExprKey, Int]
  type Resolve = Resolve.Resolve

  object Locals {
    def empty: Locals = Map.empty
  }
}
