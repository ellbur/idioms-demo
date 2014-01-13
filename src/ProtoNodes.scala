
object ProtoNodes {
  import Nodes._
  import LambdaIdiom._

  sealed trait ProtoNode {
    def :>:(s: Symbol) = ProtoLambda(s, this)
  }

  case class ProtoNodeNode(node: Node) extends ProtoNode
  case class ProtoLambda(name: Symbol, body: ProtoNode) extends ProtoNode
  case class ProtoVariable(name: Symbol) extends ProtoNode
  case class ProtoApp(car: ProtoNode, cdr: ProtoNode) extends ProtoNode

  implicit def protoizeNode(node: Node): ProtoNode = ProtoNodeNode(node)

  implicit def protoizeVariable(v: Symbol): ProtoNode = ProtoVariable(v)

  implicit class NodeProtoApply(car: Node) {
    def apply(cdr: ProtoNode) = ProtoApp(car, cdr)
  }
  implicit class ProtoProtoApply(car: ProtoNode) {
    def apply(cdr: ProtoNode) = ProtoApp(car, cdr)
  }
  implicit class SymbolProtoApply(car: Symbol) {
    def apply(cdr: ProtoNode) = ProtoApp(car, cdr)
  }

  @throws(classOf[UndefinedReference])
  def protoNodeToNode(proto: ProtoNode): Node =
    protoNodeToNode(proto, 0, Map())

  @throws(classOf[UndefinedReference])
  def protoNodeToNode(proto: ProtoNode, depth: Int, symbolTable: Map[Symbol,Int]): Node =
    proto match {
      case ProtoNodeNode(n) => Root(n)
      case ProtoLambda(name, body) =>
        val nextDepth = depth + 1
        val nextSymbolTable = symbolTable + (name -> nextDepth)
        Pure(depth + 1, lambdaIdiom, protoNodeToNode(body, nextDepth, nextSymbolTable))
      case ProtoVariable(name) =>
        symbolTable get name match {
          case None => throw UndefinedReference(name)
          case Some(index) => AntiPure(index, lambdaIdiom, Root(I))
        }
      case ProtoApp(car, cdr) =>
        App(
          protoNodeToNode(car, depth, symbolTable),
          protoNodeToNode(cdr, depth, symbolTable)
        )
    }

  case class UndefinedReference(to: Symbol) extends Exception {
    override def toString = s"Undefined reference to $to"
  }
}
