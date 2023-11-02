package com.melvic.dry.ast

import com.melvic.dry.ast.Stmt.BlockStmt
import com.melvic.dry.aux.Show
import com.melvic.dry.aux.Show.ShowInterpolator
import com.melvic.dry.aux.implicits.ListOps
import com.melvic.dry.{Show, Token}

sealed trait Expr

object Expr {
  sealed trait Literal extends Expr

  object Literal {
    final case class Number(value: Double) extends Literal
    final case class Str(value: String) extends Literal
    case object True extends Literal
    case object False extends Literal
    case object None extends Literal

    def show: Show[Literal] = {
      case Number(value) => value.toString
      case Str(value)    => s"\"$value\""
      case True          => "true"
      case False         => "false"
      case None          => "none"
    }
  }

  final case class Grouping(expr: Expr) extends Expr
  final case class Unary(operator: Token, operand: Expr) extends Expr
  final case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
  final case class Variable(name: Token) extends Expr
  final case class Assignment(name: Token, value: Expr) extends Expr

  final case class Logical(left: Expr, operator: Token, right: Expr) extends Expr

  final case class Call(callee: Expr, arguments: List[Expr], paren: Token) extends Expr
  final case class Lambda(params: List[Token], body: List[Decl]) extends Expr

  final case class Get(obj: Expr, name: Token) extends Expr
  final case class Set(obj: Expr, name: Token, value: Expr) extends Expr
  final case class IndexGet(obj: Expr, name: Token) extends Expr
  final case class IndexSet(obj: Expr, name: Token, value: Expr) extends Expr

  final case class Self(keyword: Token) extends Expr

  final case class Tuple(elems: List[Expr]) extends Expr

  final case class Dictionary(table: Map[Token, Expr]) extends Expr

  object Dictionary {
    def show: Show[Dictionary] = { case Dictionary(table) =>
      def fieldToString(field: (Token, Expr)): String =
        show"${field._1}: ${Expr.show(field._2)}"

      show"{ ${table.map(fieldToString).mkString(", ")} }"
    }
  }

  implicit def implicitShow: Show[Expr] = show

  def show: Show[Expr] = {
    case literal: Literal               => Literal.show(literal)
    case Grouping(expr)                 => show"($expr)"
    case Unary(operator, operand)       => Token.show(operator) + Expr.show(operand)
    case Binary(left, operator, right)  => show"$left $operator $right"
    case Variable(token)                => Token.show(token)
    case Assignment(name, value)        => show"$name = $value"
    case Logical(left, operator, right) => Expr.show(Binary(left, operator, right))
    case Call(callee, arguments, _)     => show"$callee(${Show.list(arguments)})"
    case Lambda(params, body) =>
      show"lambda(${params.map(Token.show).toCsv}) ${BlockStmt.fromDecls(body: _*)}"
    case Get(obj, name)             => show"$obj.$name"
    case Set(obj, name, value)      => show"$obj.$name = $value"
    case IndexGet(obj, name)        => show"$obj[$name]"
    case IndexSet(obj, name, value) => show"$obj[$name] = $value"
    case Self(_)                    => "self"
    case Tuple(elems)               => show"(${Show.list(elems)})"
    case dictionary: Dictionary     => Dictionary.show(dictionary)
  }
}
