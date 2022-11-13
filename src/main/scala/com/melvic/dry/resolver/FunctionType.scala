package com.melvic.dry.resolver

sealed trait FunctionType

object FunctionType {
  case object None extends FunctionType
  case object Function extends FunctionType
  case object Method extends FunctionType
  case object Init extends FunctionType
}
