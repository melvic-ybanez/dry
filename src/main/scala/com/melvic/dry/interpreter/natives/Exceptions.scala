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
      .defineWith(DivisionByZero.exceptionName, DException(DivisionByZero, _))
      .defineWith(UndefinedVariable.exceptionName, DException(UndefinedVariable, _))
      .defineWith(InvalidOperand.exceptionName, DException(InvalidOperand, _))
      .defineWith(InvalidOperands.exceptionName, DException(InvalidOperands, _))
      .defineWith(NotCallable.exceptionName, DException(NotCallable, _))
      .defineWith(IncorrectArity.exceptionName, DException(IncorrectArity, _))
      .defineWith(DoesNotHaveProperties.exceptionName, DException(DoesNotHaveProperties, _))
      .defineWith(UndefinedProperty.exceptionName, DException(UndefinedProperty, _))
      .defineWith(UndefinedKey.exceptionName, DException(UndefinedKey, _))
      .defineWith(CanNotApplyIndexOperator.exceptionName, DException(CanNotApplyIndexOperator, _))
      .defineWith(IndexOutOfBounds.exceptionName, DException(IndexOutOfBounds, _))
      .defineWith(InvalidIndex.exceptionName, DException(InvalidIndex, _))
      .defineWith(InvalidArgument.exceptionName, DException(InvalidArgument, _))
      .defineWith(ModuleNotFound.exceptionName, DException(ModuleNotFound, _))

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
          case DivisionByZero.`exceptionName`           => fail(RuntimeError.divisionByZero)
          case UndefinedVariable.`exceptionName`        => fail(RuntimeError.undefinedVariable)
          case InvalidOperand.`exceptionName`           => fail(RuntimeError.invalidOperand)
          case InvalidOperands.`exceptionName`          => fail(RuntimeError.invalidOperands)
          case NotCallable.`exceptionName`              => fail(RuntimeError.notCallable)
          case IncorrectArity.`exceptionName`           => fail(RuntimeError.incorrectArity)
          case DoesNotHaveProperties.`exceptionName`    => fail(RuntimeError.doesNotHaveProperties)
          case UndefinedProperty.`exceptionName`        => fail(RuntimeError.undefinedProperty)
          case UndefinedKey.`exceptionName`             => fail(RuntimeError.undefinedKey)
          case CanNotApplyIndexOperator.`exceptionName` => fail(RuntimeError.canNotApplyIndexOperator)
          case IndexOutOfBounds.`exceptionName`         => RuntimeError.indexOutOfBounds(line, message).fail
          case InvalidIndex.`exceptionName`             => fail(RuntimeError.invalidIndex)
          case InvalidArgument.`exceptionName`          => RuntimeError.invalidArgument(line, message).fail
          case ModuleNotFound.`exceptionName`           => fail(RuntimeError.moduleNotFound)
        }
      case arg :: _ => invalidArgument(arg)
    }
  }
}
