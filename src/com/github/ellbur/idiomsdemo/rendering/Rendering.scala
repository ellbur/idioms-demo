
package com.github.ellbur.idiomsdemo.rendering

import java.io.File
import org.apache.commons.io.FileUtils
import com.github.ellbur.idiomsdemo.language.Nodes._

object Rendering {
  def nodeToGraphviz(node: Node): File = {
    def nodeAttributes(node: Node) = node match {
      case _: App => s"""[shape="point",label=""]"""
      case Pure(i, _, body) => s"""[shape="circle",label="@$i"]"""
      case AntiPure(i, _, body) => s"""[shape="circe",label="$$$i"]"""
      case other => s"""[shape="box",label="${other.toString}"]"""
    }

    var nodeText: String = ""
    var edgeText: String = ""
    var index: Int = 0

    // Returns the node name.
    def process(node: Node): String = {
      val nodeName = s"n$index"
      index += 1

      nodeText += s"$nodeName ${nodeAttributes(node)};\n"
      node match {
        case App(car, cdr) =>
          val carName = process(car)
          val cdrName = process(cdr)

          edgeText += s"$nodeName -> $carName [color=black];\n"
          edgeText += s"$nodeName -> $cdrName [color=gray];\n"
        case Pure(i, _, body) =>
          val bodyName = process(body)
          edgeText += s"$nodeName -> $bodyName;\n"
        case AntiPure(i, _, body) =>
          val bodyName = process(body)
          edgeText += s"$nodeName -> $bodyName;\n"
        case IsPure(i, _, body) =>
          val bodyName = process(body)
          edgeText += s"$nodeName -> $bodyName;\n"
        case _ =>
      }

      nodeName
    }
    process(node)

    val completeText =
      s"digraph yo {\n" +
        s"    $nodeText\n" +
        s"    $edgeText\n" +
        s"}\n"

    val dotFile = File.createTempFile("hahaha", ".dot")
    dotFile.deleteOnExit()

    val imageFile = File.createTempFile("hahaha", ".svg")

    FileUtils.writeStringToFile(dotFile, completeText, "UTF-8")
    Runtime.getRuntime.exec(Array("dot", "-Tsvg", "-o", imageFile.getAbsolutePath, dotFile.getAbsolutePath)).waitFor()

    dotFile.delete()

    imageFile
  }
}
