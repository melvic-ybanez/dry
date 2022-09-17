package com.melvic.dry.interpreter.eval

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Expr
import com.melvic.dry.ast.Expr._
import com.melvic.dry.interpreter.Value.{Bool, Num, Str, None => VNone}
import com.melvic.dry.interpreter.eval.implicits._
import com.melvic.dry.interpreter.{Callable, Env, Value}
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.Result
import com.melvic.dry.result.Result.implicits.ToResult

private[eval] trait EvalExpr {
  def expr: Evaluate[Expr] = {
    case literal: Literal       => Evaluate.literal(literal)
    case Grouping(expr)         => Evaluate.expr(expr)
    case unary: Unary           => Evaluate.unary(unary)
    case binary: Binary         => Evaluate.binary(binary)
    case variable: Variable     => Evaluate.variable(variable)
    case assignment: Assignment => Evaluate.assignment(assignment)
    case logical: Logical       => Evaluate.logical(logical)
    case call: Call             => Evaluate.call(call)
  }

  def call: Evaluate[Call] = { case Call(callee, arguments, paren) =>
    Evaluate.expr(callee).flatMapValue { calleeValue =>
      def recurse(args: List[Expr], argValues: List[Value], env: Env): Result[(List[Value], Env)] =
        args match {
          case Nil => Result.succeed(argValues.reverse, env)
          case arg :: rest =>
            Evaluate.expr(arg)(env).flatMap { case (arg, env) =>
              recurse(rest, arg :: argValues, env)
            }
        }

      env =>
        recurse(arguments, Nil, env).flatMap { case (args, env) =>
          calleeValue match {
            case Callable(arity, call) =>
              if (arity == args.size) call(args)(env)
              else Result.fail(RuntimeError.incorrectArity(paren, arity, args.size))
            case _ => Result.fail(RuntimeError.notCallable(paren))
          }
        }
    }
  }

  def logical: Evaluate[Logical] = { case Logical(left, operator, right) =>
    Evaluate.expr(left).flatMapValue { left =>
      def logical(predicate: Value => Boolean): EvalResult =
        if (predicate(left)) left.env else Evaluate.expr(right)

      operator.tokenType match {
        case TokenType.Or  => logical(isTruthy)
        case TokenType.And => logical(!isTruthy(_))
      }
    }
  }

  def unary: Evaluate[Unary] = { case Unary(operator, operandTree) =>
    Evaluate.expr(operandTree).andThen {
      _.flatMapValue { operand =>
        operator match {
          case TokenType.Minus(_, _) =>
            Result.fromOption(
              operand.toNum.map(num => Num(-num.value)),
              RuntimeError.invalidOperand(operator, "number" :: Nil)
            )
          case TokenType.Not(_, _) => Bool(!isTruthy(operand)).ok
          case _                   => VNone.ok
        }
      }
    }
  }

  def binary: Evaluate[Binary] = { case Binary(leftTree, operator @ Token(operatorType, _, _), rightTree) =>
    env =>
      def fromValueOperands(left: Value, right: Value): Result[Value] = {
        def binary[O, V](fold: (Double, Double) => O, toValue: O => V): Result[V] =
          Result.fromOption(
            for {
              leftNum  <- left.toNum
              rightNum <- right.toNum
            } yield toValue(fold(leftNum.value, rightNum.value)),
            RuntimeError.invalidOperands(operator, "number" :: Nil)
          )

        def combine(f: (Double, Double) => Result[Double]): Result[Num] =
          binary(f, (result: Result[Double]) => result.map(Num)).flatten

        def combineUnsafe(f: (Double, Double) => Double): Result[Num] =
          combine((x, y) => f(x, y).ok)

        def compare(f: (Double, Double) => Boolean): Result[Bool] =
          binary(f, Bool)

        def bitwise(f: (Long, Long) => Long): Result[Num] =
          combineUnsafe { case (x, y) => f(x.toLong, y.toLong) }

        operatorType match {
          case TokenType.Plus =>
            (left, right) match {
              case (Num(l), Num(r)) => Num(l + r).ok
              case (Str(l), Str(r)) => Str(l + r).ok
              case _ =>
                val error = RuntimeError.invalidOperands(operator, List("number", "string"))
                Result.fail(error)
            }
          case TokenType.Minus => combineUnsafe(_ - _)
          case TokenType.Star  => combineUnsafe(_ * _)
          case TokenType.Slash =>
            combine {
              case (_, 0) => Result.fail(RuntimeError.divisionByZero(operator))
              case (x, y) => (x / y).ok
            }
          case TokenType.Modulo       => combineUnsafe(_ % _)
          case TokenType.BAnd         => bitwise(_ & _)
          case TokenType.BOr          => bitwise(_ | _)
          case TokenType.BXor         => bitwise(_ ^ _)
          case TokenType.LeftShift    => bitwise(_ << _)
          case TokenType.RightShift   => bitwise(_ >> _)
          case TokenType.URightShift  => bitwise(_ >>> _)
          case TokenType.Greater      => compare(_ > _)
          case TokenType.GreaterEqual => compare(_ >= _)
          case TokenType.Less         => compare(_ < _)
          case TokenType.LessEqual    => compare(_ <= _)
          case TokenType.NotEqual     => compare(_ != _)
          case TokenType.EqualEqual   => compare(_ == _)
          case _                      => VNone.ok
        }
      }

      for {
        left   <- Evaluate.expr(leftTree)(env)
        right  <- Evaluate.expr(rightTree)(left.env)
        result <- fromValueOperands(left.value, right.value).withEnv(right.env)
      } yield result
  }

  def literal: Evaluate[Literal] = {
    case Literal.True          => Bool(true).env
    case Literal.False         => Bool(false).env
    case Literal.None          => VNone.env
    case Literal.Number(value) => Num(value).env
    case Literal.Str(string)   => Str(string).env
  }

  def variable: Evaluate[Variable] = { case Variable(expr) =>
    toEvalResult(Env.get(expr))
  }

  def assignment: Evaluate[Assignment] = { case Assignment(name, value) =>
    Evaluate
      .expr(value)
      .andThen(_.flatMap { case (value, env) =>
        env.assign(name, value).map((value, _))
      })
  }

}