package com.melvic.dry.interpreter.eval

import com.melvic.dry.Token
import com.melvic.dry.Token.TokenType
import com.melvic.dry.ast.Expr
import com.melvic.dry.ast.Expr.{List => _, _}
import com.melvic.dry.aux.implicits.ListOps
import com.melvic.dry.interpreter.Interpret
import com.melvic.dry.interpreter.Value.{Bool, Num, Str, None => VNone}
import com.melvic.dry.interpreter.eval.Context.implicits._
import com.melvic.dry.interpreter.eval.Evaluate.Out
import com.melvic.dry.interpreter.values.Callable.Varargs
import com.melvic.dry.interpreter.values._
import com.melvic.dry.resolver.LocalExprKey
import com.melvic.dry.result.Failure.RuntimeError
import com.melvic.dry.result.Result
import com.melvic.dry.result.Result.Result
import com.melvic.dry.result.Result.implicits._

import scala.annotation.nowarn
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

//noinspection ScalaWeakerAccess
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
    case indexGet: IndexGet     => Evaluate.indexGet(indexGet)
    case indexSet: IndexSet     => Evaluate.indexSet(indexSet)
    case self: Self             => Evaluate.self(self)
    case list: Expr.List        => Evaluate.list(list)
    case tuple: Tuple           => Evaluate.tuple(tuple)
    case dictionary: Dictionary => Evaluate.dictionary(dictionary)
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
        def unaryForNum(f: Double => Double) =
          Result.fromOption(
            operand.toNum.map(num => Num(f(num.value))),
            RuntimeError.invalidOperand(node.operator, "number" :: Nil)
          )

        node.operator match {
          case TokenType.Minus(_, _) => unaryForNum(-_)
          case TokenType.Plus(_, _)  => unaryForNum(identity)
          case TokenType.Not(_, _)   => Bool(!isTruthy(operand)).ok
          case _                     => VNone.ok
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
          case TokenType.NotEqual     => (left != right).ok.map(Value.Bool)
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
      Evaluate.expr(obj).flatMap {
        case instance: DObject => Evaluate.expr(value).map(instance.set(name, _))
        case module: DModule   => Evaluate.expr(value).map(module.set(name, _))
        case _                 => RuntimeError.doesNotHaveProperties(obj, name).fail[Value]
      }
  }

  def indexGet(implicit context: Context[IndexGet]): Out = node match {
    case IndexGet(obj, key, token) =>
      Evaluate.index(obj, key, token) {
        case (dict: DDictionary, evaluatedKey) =>
          Result.fromOption(dict.getByKey(evaluatedKey), RuntimeError.undefinedKey(key, token))
        case (sequence: DSequence, evaluatedKey) =>
          evaluatedKey match {
            case Value.Num(index) if index % 1 == 0 =>
              val intIndex = index.toInt
              if (index < 0 || index >= sequence.size)
                RuntimeError.indexOutOfBounds(intIndex, token.line).fail[Value]
              else sequence.getByIndex(intIndex).ok
            case _ => RuntimeError.invalidIndex(key, token).fail[Value]
          }
      }
  }

  def indexSet(implicit context: Context[IndexSet]): Out = node match {
    case IndexSet(obj, key, value, token) =>
      Evaluate.index(obj, key, token) { case (dict: DDictionary, evaluatedKey) =>
        Evaluate.expr(value).map(dict.setByKey(evaluatedKey, _))
      }
  }

  def self(implicit context: Context[Self]): Out = varLookup(node.keyword, node)

  def list(implicit context: Context[Expr.List]): Out = node match {
    case Expr.List(elems) =>
      Evaluate.exprList(elems).map(elems => DList(elems.reverse.to(ListBuffer), env))
  }

  def tuple(implicit context: Context[Tuple]): Out = node match {
    case Tuple(elems) => exprList(elems).map(elems => DTuple(elems.reverse, env))
  }

  def dictionary(implicit context: Context[Dictionary]): Out = node match {
    case Dictionary(table) =>
      val dictFields = table.toList.foldFailFast(Result.succeed(Map.empty[Value, Value])) {
        case (result, (key, value)) =>
          for {
            evaluatedKey   <- key.fold(Evaluate.literal(_), Evaluate.unary(_))
            evaluatedValue <- Evaluate.expr(value)
          } yield result + (evaluatedKey -> evaluatedValue)
      }

      dictFields.map(fields => DDictionary(fields.to(mutable.Map), env))
  }

  private[eval] def exprList(elems: List[Expr])(implicit context: Context[Expr]): Result[List[Value]] =
    elems.foldFailFast(Result.succeed(List.empty[Value])) { (result, elem) =>
      Evaluate.expr(elem).map(_ :: result)
    }

  private[eval] def index[A](obj: Expr, key: Expr.IndexKey, token: Token)(
      ifCanBeIndexed: PartialFunction[(Value, Value), Out]
  )(implicit context: Context[A]): Out = {
    for {
      evaluatedObj <- Evaluate.expr(obj)
      evaluatedKey <- key.fold(Evaluate.literal(_), Evaluate.unary(_))
      orElse: PartialFunction[(Value, Value), Out] = { case (_: Value, _: Value) =>
        RuntimeError.canNotApplyIndexOperator(obj, token).fail[Value]
      }
      result <- ifCanBeIndexed.orElse(orElse)(evaluatedObj, evaluatedKey)
    } yield result
  }

  private def varLookup(name: Token, expr: Expr)(implicit context: Context[Expr]): Out =
    locals
      .get(LocalExprKey(expr))
      .flatMap(distance => env.at(distance, name.lexeme))
      .fold(Interpret.natives.get(name))(_.ok)
}
