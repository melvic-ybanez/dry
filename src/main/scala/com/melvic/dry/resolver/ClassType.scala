package com.melvic.dry.resolver

sealed trait ClassType

object ClassType {
  case object None extends ClassType
  case object Class extends ClassType
}
