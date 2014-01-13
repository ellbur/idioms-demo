
object Annotated extends App {
  sealed trait Node

  case object S extends Node
  case object K extends Node
  case object I extends Node

  case class Pure(i: Int) extends Node {
    override def toString = s"@$i"
  }
  case class AntiPure(i: Int, idiom: Idiom) extends Node {
    override def toString = "$" + i
  }

  case class App(car: Node, cdr: Node) extends Node {
    override def toString = s"$car($cdr)"
  }

  case class Idiom(pure: Node, ap: Node)

  case class AnnotatedNode(node: Node, annotation: Node)

}
