
package com.github.ellbur.idiomsdemo.language

object Nodes {
  sealed trait Node

  // Basic combinators
  case object S extends Node
  case object K extends Node
  case object I extends Node
  case object Y extends Node

  // Maybe
  case object Nothing extends Node
  case object Just extends Node
  case object MatchMaybe extends Node

  // Idioms
  case class Pure(i: Int, idiom: Idiom, body: Node) extends Node {
    override def toString = s"@$i($body)"
  }
  case class AntiPure(i: Int, idiom: Idiom, body: Node) extends Node {
    override def toString = "$" + i
  }
  case class IsPure(i: Int, idiom: Idiom, body: Node) extends Node {
    override def toString = s"[$i]"
  }

  // The purity in the leaves of the tree
  case class Root(leaf: Node) extends Node {
  }

  // Function application
  case class App(car: Node, cdr: Node) extends Node {
    override def toString = s"$car($cdr)"
  }

  case class Idiom(pure: Node, ap: Node)
}
