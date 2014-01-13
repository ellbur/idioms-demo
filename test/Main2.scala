
object Main2 extends App {
  sealed trait Node

  case object S extends Node
  case object K extends Node
  case object I extends Node

  case object Nothing extends Node
  case object Just extends Node
  case object MatchMaybe extends Node

  case object Nil extends Node
  case object Cons extends Node
  case object MatchList extends Node

  case class Pure(i: Int) extends Node {
    override def toString = s"@$i"
  }
  case class AntiPure(i: Int, idiom: Idiom) extends Node {
    override def toString = "$" + i
  }

  case class App(car: Node, cdr: Node) extends Node {
    override def toString = s"$car($cdr)"
  }

  object * {
    def unapply(node: Node): Option[(Node, Node)] = node match {
      case App(car, cdr) => Some((car, cdr))
      case _ => None
    }
  }

  object $ {
    def apply(i: Int, idiom: Idiom) = AntiPure(i, idiom)
    def unapply(n: Node): Option[(Int, Idiom)] = n match {
      case AntiPure(i, idiom) => Some((i, idiom))
      case _ => None
    }
  }

  case class Idiom(pure: Node, ap: Node)

  object NodeApply {
    implicit class NodeApplyApply(car: Node) {
      def apply(cdr: Node) = App(car, cdr)
    }
  }

  def reduceOnceHere(node: Node): Option[Node] = {
    import NodeApply._

    node match {
      // Basic combinators
      case I*x => Some(x)
      case K*a*b => Some(a)
      case S*a*b*c => Some(a(c)(b(c)))

      // Cancellation
      case Pure(i1) * ($(i2, _)*x) if i1 == i2 => Some(x)

      // Break through right
      case y * ($(i, idiom) * x) if passesThrough(y, i) =>
        Some($(i, idiom)(idiom.ap(idiom.pure(y))(x)))

      // Specific data types
      case MatchMaybe*Nothing*f*g => Some(f)
      case MatchMaybe*(Just*x)*f*g => Some(g(x))

      case MatchList*Nil*f*g => Some(f)
      case MatchList*(Cons*x*y)*f*g => Some(g(x)(y))

      case _ => None
    }
  }

  def passesThrough(node: Node, i: Int): Boolean = node match {
    case K => true

    case AntiPure(i2, _) if i > i2 => true

    case Just => true

    case _ => false
  }

  def reduceOnce(node: Node): Option[Node] = {
    import NodeApply._
    reduceOnceHere(node) match {
      case Some(next) => Some(next)
      case None =>
        node match {
          case App(car, cdr) =>
            reduceOnce(car) match {
              case Some(nextCar) => Some(nextCar(cdr))
              case None =>
                reduceOnce(cdr) match {
                  case Some(nextCdr) => Some(car(nextCdr))
                  case None => None
                }
            }
          case _ => None
        }
    }
  }

  def step(expr: Node) {
    println(expr)
    reduceOnce(expr) foreach step
  }

  sealed trait ProtoNode {
    def :>:(s: Symbol) = ProtoLambda(s, this)
  }

  case class ProtoNodeNode(node: Node) extends ProtoNode
  case class ProtoLambda(name: Symbol, body: ProtoNode) extends ProtoNode
  case class ProtoVariable(name: Symbol) extends ProtoNode
  case class ProtoApp(car: ProtoNode, cdr: ProtoNode) extends ProtoNode

  implicit def protoizeNode(node: Node): ProtoNode = ProtoNodeNode(node)

  implicit def protoizeVariable(v: Symbol): ProtoNode = ProtoVariable(v)

  {
    implicit class NodeProtoApply(car: Node) {
      def apply(cdr: ProtoNode) = ProtoApp(car, cdr)
    }
    implicit class ProtoProtoApply(car: ProtoNode) {
      def apply(cdr: ProtoNode) = ProtoApp(car, cdr)
    }
    implicit class SymbolProtoApply(car: Symbol) {
      def apply(cdr: ProtoNode) = ProtoApp(car, cdr)
    }

    val matchMaybe = MatchMaybe

    val maybeAp = 'mx :>: 'my :>:
      matchMaybe('mx)(
        Nothing
      )(
        'x :>: matchMaybe('my)(
          Nothing
        )(
          'y :>: 'x('y)
        )
      )
  }
}
