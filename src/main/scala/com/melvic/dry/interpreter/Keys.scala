package com.melvic.dry.interpreter

object Keys {
  val TestCount = "__tests_count__"
  val SuccessCount = "__tests_success_count__"
  val LineNumber = "__line_number__"
  val MainModule = "__main_module__"

  object Errors {
    val DivisionByZero = "__division_by_zero__"
    val InvalidOperand = "__invalid_operand__"
    val InvalidOperands = "__invalid_operands__"
    val UndefinedVariable = "__undefined_variable__"
    val NotCallable = "__not_callable__"
    val IncorrectArity = "__incorrect_arity__"
    val DoesNotHaveProperties = "__does_not_have_properties__"
    val UndefinedProperty = "__undefined_property__"
    val IndexOutOfBounds = "__index_out_of_bounds__"
    val InvalidIndex = "__invalid_index__"
    val InvalidArgument = "__invalid_argument__"
    val ModuleNotFound = "__module_not_found__"
  }
}
