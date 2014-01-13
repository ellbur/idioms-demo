
scalaVersion := "2.11.0-M6"

scalaSource in Compile <<= baseDirectory(_ / "src")

javaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test")

javaSource in Test <<= baseDirectory(_ / "test")

resourceDirectory in Compile <<= baseDirectory(_ / "resources")

ideaExcludeFolders ++= Seq(
    ".idea", ".idea_modules", ".settings", ".worksheet", "bin", "project"
)

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-io" % "1.3.2",
  "org.scala-lang.modules" % "scala-xml_2.11.0-M6" % "1.0.0-RC6"
)
