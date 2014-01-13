
import com.github.ellbur.idiomsdemo.language._
import Nodes._
import ProtoNodes._
import Reductions._
import com.github.ellbur.idiomsdemo.rendering.Rendering._
import java.io.{FileWriter, PrintWriter, File}

object Test1 extends scala.App {
  {
    val just = protoNodeToNode('x :>: (Just)('x))
    val double = protoNodeToNode('f :>: 'x :>: 'f('f('x)))

    val joinMaybe =
      'mmx :>:
        (MatchMaybe)('mmx)(
          Nothing
        )(
          'mx :>:
            (MatchMaybe)('mx)(
              Nothing
            )(
              Just
            )
        )

    val exprNode = App(protoNodeToNode(joinMaybe), App(App(double, just), K))

    //val exprNode = protoNodeToNode(expr)

    def step(node: Node): List[(Node, String)] =
      reduceOnce(node) match {
        case None => Nil
        case Some((next, reason)) => (next, reason) :: step(next)
      }

    val steps = (exprNode, "Initial") :: step(exprNode)

    val htmlFile = new File("/tmp/yo.html")
    val pw = new PrintWriter(new FileWriter(htmlFile))

    pw.print(
      <html>
        <head>
          <title>Oh dear...</title>
          <style type="text/css">
            img {{
              padding: 30px 30px 30px 30px;
            }}
            figure, figcaption, img {{
              display: inline;
            }}
          </style>
        </head>
        <body>
          {
            steps map { case (step, reason) =>
              val file = nodeToGraphviz(step)
              <figure>
                <figcaption>{reason}</figcaption>
                <img src={file.toURI.toString}/>
              </figure>
            }
          }
        </body>
      </html>.toString()
    )
    pw.flush()
    pw.close()
  }
}
