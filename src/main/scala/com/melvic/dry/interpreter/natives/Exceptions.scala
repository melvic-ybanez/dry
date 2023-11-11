package com.melvic.dry.interpreter.natives

import com.melvic.dry.Token
import com.melvic.dry.interpreter.Env
import com.melvic.dry.interpreter.Env.Register
import com.melvic.dry.interpreter.values.Value.{Types, typeOf}
import com.melvic.dry.interpreter.values.{Callable, DException, DInstance, Value}
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Failure.RuntimeError.Kind._
import com.melvic.dry.result.Result.Result

object Exceptions {
  def register: Register =
    _.defineWith("raise", raise)
      .defineWith(DivisionByZero.name, DException(DivisionByZero, _))
      .defineWith(UndefinedVariable.name, DException(UndefinedVariable, _))
      .defineWith(InvalidOperand.name, DException(InvalidOperand, _))
      .defineWith(InvalidOperands.name, DException(InvalidOperands, _))
      .defineWith(NotCallable.name, DException(NotCallable, _))
      .defineWith(IncorrectArity.name, DException(IncorrectArity, _))
      .defineWith(DoesNotHaveProperties.name, DException(DoesNotHaveProperties, _))
      .defineWith(UndefinedProperty.name, DException(UndefinedProperty, _))
      .defineWith(UndefinedKey.name, DException(UndefinedKey, _))
      .defineWith(CanNotApplyIndexOperator.name, DException(CanNotApplyIndexOperator, _))
      .defineWith(IndexOutOfBounds.name, DException(IndexOutOfBounds, _))
      .defineWith(InvalidIndex.name, DException(InvalidIndex, _))
      .defineWith(InvalidArgument.name, DException(InvalidArgument, _))
      .defineWith(ModuleNotFound.name, DException(ModuleNotFound, _))

  private def raise(env: Env): Callable = Callable.withLineNo(1, env) { line =>
    def invalidArgument(got: Value): Result[Value] =
      RuntimeError.invalidArgument(Types.Exception, typeOf(got), line).fail

    {
      case (exception: DInstance) :: _ =>
        def message: String =
          DException.messageOf(exception).getOrElse("An exception occurred")

        def fail(error: (Token, String) => RuntimeError) =
          error(Token.fromLine(line), message).fail

        DException.kindOf(exception).fold(invalidArgument(exception)) {
          case DivisionByZero.name           => fail(RuntimeError.divisionByZero)
          case UndefinedVariable.name        => fail(RuntimeError.undefinedVariable)
          case InvalidOperand.name           => fail(RuntimeError.invalidOperand)
          case InvalidOperands.name          => fail(RuntimeError.invalidOperands)
          case NotCallable.name              => fail(RuntimeError.notCallable)
          case IncorrectArity.name           => fail(RuntimeError.incorrectArity)
          case DoesNotHaveProperties.name    => fail(RuntimeError.doesNotHaveProperties)
          case UndefinedProperty.name        => fail(RuntimeError.undefinedProperty)
          case UndefinedKey.name             => fail(RuntimeError.undefinedKey)
          case CanNotApplyIndexOperator.name => fail(RuntimeError.canNotApplyIndexOperator)
          case IndexOutOfBounds.name         => RuntimeError.indexOutOfBounds(line, message).fail
          case InvalidIndex.name             => fail(RuntimeError.invalidIndex)
          case InvalidArgument.name          => RuntimeError.invalidArgument(line, message).fail
          case ModuleNotFound.name           => fail(RuntimeError.moduleNotFound)
        }
      case arg :: _ => invalidArgument(arg)
    }
  }
}
