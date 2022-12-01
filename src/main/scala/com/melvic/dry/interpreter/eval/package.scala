package com.melvic.dry.interpreter

import com.melvic.dry.interpreter.Value.{Bool, Str, None => VNone}
import com.melvic.dry.interpreter.values.Value

package object eval {
  def isTruthy(value: Value): Boolean =
    value match {
      case VNone       => false
      case Str("")     => false
      case Bool(value) => value
      case _           => true
    }
}
