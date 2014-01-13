
object Main extends App {
  sealed trait Node {
    def apply(cdr: Node) = App(this, cdr)
  }

  case object S extends Node
  case object K extends Node
  case object I extends Node

  case object N extends Node
  case object J extends Node
  case object M extends Node

  case object X extends Node
  case object Y extends Node

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

  def reduceOnceHere(node: Node): Option[Node] = node match {
    // Basic combinators
    case App(I, x) => Some(x)
    case App(App(K, a), b) => Some(a)
    case App(App(App(S, a), b), c) => Some(a(c)(b(c)))

    // Native option support so I can read it
    case App(App(M, App(J, a)), App(J, b)) => Some(J(a(b)))
    case App(App(M, N), _) => Some(N)
    case App(App(M, _), N) => Some(N)

    // Cancellation
    case App(Pure(i1), App(AntiPure(i2, _), x)) if i1 == i2 => Some(x)

    // Break through right
    case App(y, App(a@AntiPure(i, idiom), x)) if passesThrough(y, i) =>
      Some(App(a, idiom.ap(idiom.pure(y))(x)))

    case _ => None
  }

  def passesThrough(node: Node, i: Int): Boolean = node match {
    case K => true

    case AntiPure(i2, _) if i > i2 => true

    case J => true

    case _ => false
  }

  def reduceOnce(node: Node): Option[Node] =
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

  def step(expr: Node) {
    println(expr)
    reduceOnce(expr) foreach step
  }

  val func = Idiom(K, S)
  def lam(i: Int) = Pure(i)
  def v(i: Int) = AntiPure(i, func)

  val opt = Idiom(J, M)
  def lamo(i: Int) = Pure(i)
  def vo(i: Int) = AntiPure(i, opt)

  val expr = lamo(1)(lamo(2)(vo(1)(vo(2)( J(J(X)) ))))

  step(expr)
}
