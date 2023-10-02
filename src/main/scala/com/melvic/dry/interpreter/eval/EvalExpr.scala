package com.melvic.dry.interpreter.eval

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Expr
import com.melvic.dry.ast.Expr._
import com.melvic.dry.interpreter.Interpret
import com.melvic.dry.interpreter.Value.{Bool, Num, Str, None => VNone}
import com.melvic.dry.interpreter.eval.Context.implicits._
import com.melvic.dry.interpreter.eval.Evaluate.Out
import com.melvic.dry.interpreter.values.Callable.Varargs
import com.melvic.dry.interpreter.values.{Callable, DModule, DObject, Value}
import com.melvic.dry.resolver.LocalExprKey
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.Result
import com.melvic.dry.result.Result.implicits._

import scala.annotation.nowarn

private[eval] trait EvalExpr {
  def expr(implicit context: Context[Expr]): Out = node match {
    case lambda: Lambda         => Evaluate.lambda(lambda)
    case literal: Literal       => Evaluate.literal(literal)
    case Grouping(expr)         => Evaluate.expr(expr)
    case unary: Unary           => Evaluate.unary(unary)
    case binary: Binary         => Evaluate.binary(binary)
    case variable: Variable     => Evaluate.variable(variable)
    case assignment: Assignment => Evaluate.assignment(assignment)
    case logical: Logical       => Evaluate.logical(logical)
    case call: Call             => Evaluate.call(call)
    case get: Get               => Evaluate.get(get)
    case set: Set               => Evaluate.set(set)
    case self: Self             => Evaluate.self(self)
  }

  def lambda(implicit context: Context[Lambda]): Out =
    Callable.Lambda(node, env, locals, sourcePaths).ok

  def call(implicit context: Context[Call]): Out = node match {
    case Call(callee, arguments, paren) =>
      Evaluate.expr(callee).flatMap { calleeValue =>
        def recurse(args: List[Expr], argValues: List[Value]): Result[List[Value]] =
          args match {
            case Nil => Result.succeed(argValues.reverse)
            case arg :: rest =>
              Evaluate.expr(arg).flatMap { arg =>
                recurse(rest, arg :: argValues)
              }
          }

        recurse(arguments, Nil).flatMap { args =>
          calleeValue match {
            case callable: Varargs => callable.callWithPos(paren)(args)
            case Callable(arity, _, callWithToken) =>
              if (arity == args.size) callWithToken(paren)(args)
              else Result.fail(RuntimeError.incorrectArity(paren, arity, args.size))
            case _ => Result.fail(RuntimeError.notCallable(paren))
          }
        }
      }
  }

  def logical(implicit context: Context[Logical]): Out = node match {
    case Logical(left, operator, right) =>
      Evaluate.expr(left).flatMap { left =>
        def logical(predicate: Value => Boolean): Out =
          if (predicate(left)) left.ok else Evaluate.expr(right)

        @nowarn
        val result = operator.tokenType match {
          case TokenType.Or  => logical(isTruthy)
          case TokenType.And => logical(!isTruthy(_))
        }

        result
      }
  }

  def unary(implicit context: Context[Unary]): Out =
    Evaluate
      .expr(node.operand)
      .flatMap { operand =>
        node.operator match {
          case TokenType.Minus(_, _) =>
            Result.fromOption(
              operand.toNum.map(num => Num(-num.value)),
              RuntimeError.invalidOperand(node.operator, "number" :: Nil)
            )
          case TokenType.Not(_, _) => Bool(!isTruthy(operand)).ok
          case _                   => VNone.ok
        }
      }

  def binary(implicit context: Context[Binary]): Out = node match {
    case Binary(leftTree, operator @ Token(operatorType, _, _), rightTree) =>
      def fromValueOperands(left: Value, right: Value): Out = {
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
          combineUnsafe { case (x, y) => f(x.toLong, y.toLong).toDouble }

        operatorType match {
          case TokenType.Plus =>
            (left, right) match {
              case (Num(l), Num(r)) => Num(l + r).ok
              case (Str(l), Str(r)) => Str(l + r).ok
              case _                => RuntimeError.invalidOperands(operator, List("number", "string")).fail
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
          case TokenType.EqualEqual   => (left == right).ok.map(Value.Bool)
          case _                      => VNone.ok
        }
      }

      for {
        left   <- Evaluate.expr(leftTree)
        right  <- Evaluate.expr(rightTree)
        result <- fromValueOperands(left, right)
      } yield result
  }

  def literal(implicit context: Context[Literal]): Out = node match {
    case Literal.True          => Bool(true).ok
    case Literal.False         => Bool(false).ok
    case Literal.None          => VNone.ok
    case Literal.Number(value) => Num(value).ok
    case Literal.Str(string)   => Str(string).ok
  }

  def variable(implicit context: Context[Variable]): Out = varLookup(node.name, node)

  def assignment(implicit context: Context[Assignment]): Out =
    Evaluate
      .expr(node.value)
      .flatMap { value =>
        locals
          .get(LocalExprKey(node))
          .map(distance => env.assignAt(distance, node.name, value))
          .fold(Interpret.natives.assign(node.name, value))(_.ok)
          .map(_ => value)
      }

  def get(implicit context: Context[Get]): Out = node match {
    case Get(obj, name) =>
      Evaluate
        .expr(obj)
        .flatMap {
          case instance: DObject => instance.get(name)
          case module: DModule   => module.get(name)
          case _                 => RuntimeError.doesNotHaveProperties(obj, name).fail
        }
  }

  def set(implicit context: Context[Set]): Out = node match {
    case Set(obj, name, value) =>
      Evaluate
        .expr(obj)
        .flatMap {
          case instance: DObject => Evaluate.expr(value).map(instance.set(name, _))
          case module: DModule   => Evaluate.expr(value).map(module.set(name, _))
          case _                 => RuntimeError.doesNotHaveProperties(obj, name).fail[Value]
        }
  }

  def self(implicit context: Context[Self]): Out = varLookup(node.keyword, node)

  private def varLookup(name: Token, expr: Expr)(implicit context: Context[Expr]): Out =
    locals
      .get(LocalExprKey(expr))
      .flatMap(distance => env.at(distance, name.lexeme))
      .fold(Interpret.natives.get(name))(_.ok)
}
