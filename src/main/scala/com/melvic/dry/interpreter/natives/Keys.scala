package com.melvic.dry.interpreter.natives

import com.melvic.dry.result.Failure.RuntimeError

object Keys {
  val TestCount = "__tests_count__"
  val SuccessCount = "__tests_success_count__"
  val MainModule = "__main_module__"

  object Errors {
    // TODO: We might no longer need this once the native `assert_error` function is removed
    def fromErrorKind(kind: RuntimeError.Kind): String = {
      val name = s"${kind.name.head.toLower}${kind.name.tail}"
      val snakeCase = name.foldLeft("")((name, c) => name + (if (c.isUpper) "_" + c.toLower else c))
      s"__${snakeCase}__"
    }

    def allErrors: List[String] =
      RuntimeError.Kind.all.map(fromErrorKind)
  }
}
