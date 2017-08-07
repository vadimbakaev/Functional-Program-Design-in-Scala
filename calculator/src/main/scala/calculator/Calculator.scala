package calculator

import scala.annotation.tailrec

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
                     namedExpressions: Map[String, Signal[Expr]]
                   ): Map[String, Signal[Double]] = {
    namedExpressions.map { case (key, expr) => (key, Signal(eval(expr(), namedExpressions))) }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) =>
        v
      case _          =>
        evalWithHistory(Set(expr), expr, references)
    }
  }

  def evalWithHistory(resolved: Set[Expr], toResolve: Expr, references: Map[String, Signal[Expr]]): Double = {
    val newResolved = resolved + toResolve
    toResolve match {
      case Literal(v)   =>
        v
      case Ref(name)    =>
        if (resolved.contains(toResolve)) {
          Double.NaN
        } else {
          evalWithHistory(newResolved, getReferenceExpr(name, references), references)
        }
      case Plus(a, b)   =>
        evalWithHistory(newResolved, a, references) +
          evalWithHistory(newResolved, b, references)
      case Minus(a, b)  =>
        evalWithHistory(newResolved, a, references) -
          evalWithHistory(newResolved, b, references)
      case Times(a, b)  =>
        evalWithHistory(newResolved, a, references) *
          evalWithHistory(newResolved, b, references)
      case Divide(a, b) =>
        evalWithHistory(newResolved, a, references) /
          evalWithHistory(newResolved, b, references)
    }
  }

  @tailrec
  private def resolve(resolved: Set[Expr], toResolve: Expr, references: Map[String, Signal[Expr]]): Expr =
    toResolve match {
      case Ref(name) =>
        val newExpr = getReferenceExpr(name, references)
        if (resolved.contains(newExpr)) {
          Literal(Double.NaN)
        } else {
          resolve(resolved + newExpr, newExpr, references)
        }
      case _         =>
        toResolve
    }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]
                              ): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
