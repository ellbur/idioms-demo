
package com.github.ellbur.idiomsdemo.language

object Reductions {
  import Nodes._

  private implicit class NodeApplyApply(car: Node) {
    def apply(cdr: Node) = App(car, cdr)
  }

  private object * {
    def unapply(node: Node): Option[(Node, Node)] = node match {
      case App(car, cdr) => Some((car, cdr))
      case _ => None
    }
  }

  // The returned string is the output that may have been produced by the
  // reduction.
  def reduceOnceHere(node: Node, pureStack: List[(Int, Idiom)]): Option[(Node, String)] = {
    implicit def justTheBasics(node: (Node, String)): Option[(Node, String)] =
      Some(node)

    node match {
      // Basic combinators
      case I*x => (x, "Reduce I")
      case K*a*b => (a, "Reduce K")
      case S*a*b*c => (a(c)(b(c)), "Reduce S")

      // Eta-expanded Y combinator prevents weird accidents.
      case Y*f*x => (f((Y)(f))(x), "Reduce Y")

      // Cancellation
      case Pure(i1, _, AntiPure(i2, _, IsPure(i3, _, x))) if i1==i2 && i1==i3 => (x, s"Cancel $i1")
      case Pure(i1, idiom, IsPure(i2, _, x)) if i1 == i2 => (idiom.pure(x), s"Cancel $i1")

      // Break through right
      case IsPure(i1, idiom1, x) * AntiPure(i2, idiom2, y) if i1==i2 =>
        (AntiPure(i1, idiom1, idiom1.ap(idiom1.pure(IsPure(i1, idiom1, x)))(y)), s"Break $i1 right")

      // Break through left
      case AntiPure(i1, idiom1, x) * IsPure(i2, idiom2, y) if i1==i2 =>
        (AntiPure(i1, idiom1, idiom1.ap(x)(idiom1.pure(IsPure(i1, idiom1, y)))), s"Break $i1 left")

      // Harmonize
      case AntiPure(i1, idiom1, x) * AntiPure(i2, idiom2, y) if i1==i2 =>
        (AntiPure(i1, idiom1, idiom1.ap(x)(y)), s"Harmonize $i1")

      // App purity
      case IsPure(i1, idiom1, x) * IsPure(i2, idiom2, y) if i1==i2 =>
        (IsPure(i1, idiom1, x(y)), s"App $i1 purity")

      // AntiPure purity
      case AntiPure(i1, idiom1, IsPure(i2, idiom2, x)) if i2 > i1 =>
        (IsPure(i2, idiom2, AntiPure(i1, idiom1, x)), s"$i2 through $i1 purity")

      // Root purity
      case Root(x) =>
        def addAll(pureStack: List[(Int, Idiom)]): Node =
          pureStack match {
            case Nil => x
            case (i, idiom) :: rest =>
              IsPure(i, idiom, addAll(rest))
          }
        (addAll(pureStack), s"$x root purity")

      // Specific data types
      case MatchMaybe*Nothing*f*g => (f, s"Match Nothing")
      case MatchMaybe*(Just*x)*f*g => (g(x), s"Match Just")

      case _ => None
    }
  }

  def reduceOnce(node: Node): Option[(Node, String)] =
    reduceOnce(node, Nil)

  def reduceOnce(node: Node, pureStack: List[(Int, Idiom)]): Option[(Node, String)] = {
    reduceOnceHere(node, pureStack) match {
      case Some((next, reason)) => Some((next, reason))
      case None =>
        node match {
          case App(car, cdr) =>
            reduceOnce(car, pureStack) match {
              case Some((nextCar, reason)) => Some((nextCar(cdr), reason))
              case None =>
                reduceOnce(cdr, pureStack) match {
                  case Some((nextCdr, reason)) => Some((car(nextCdr), reason))
                  case None => None
                }
            }
          case Pure(i, idiom, body) =>
            reduceOnce(body, (i, idiom) :: pureStack) match {
              case Some((nextBody, reason)) => Some((Pure(i, idiom, nextBody), reason))
              case None => None
            }
          case AntiPure(i, idiom, body) =>
            reduceOnce(body, pureStack) match {
              case Some((nextBody, reason)) => Some((AntiPure(i, idiom, nextBody), reason))
              case None => None
            }
          case IsPure(i, idiom, body) =>
            reduceOnce(body, pureStack filter (_._1 != i)) match {
              case Some((nextBody, reason)) => Some((IsPure(i, idiom, nextBody), reason))
              case None => None
            }
          case _ => None
        }
    }
  }

  def reduceFully(node: Node): Node =
    reduceOnce(node) match {
      case None => node
      case Some((next, _)) => reduceFully(next)
    }
}
