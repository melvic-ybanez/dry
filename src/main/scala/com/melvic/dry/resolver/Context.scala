package com.melvic.dry.resolver

final case class Context(
    scopes: List[Scope],
    locals: Locals,
    functionType: FunctionType,
    classType: ClassType
) {
  def withFunctionType(functionType: FunctionType): Context =
    copy(functionType = functionType)

  def withClassType(classType: ClassType): Context =
    copy(classType = classType)
}

object Context {
  val default: Context = Context(Nil, Map(), FunctionType.None, ClassType.None)
}
