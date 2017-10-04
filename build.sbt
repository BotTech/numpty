import Common._
import Dependencies._

lazy val root = commonProject("root", file(".")).settings(
  name := "checkity"
).aggregate(base, core, macros)

lazy val base = commonProject("base", file("checkity-base")).settings(
  libraryDependencies += scalaTest
)

lazy val macros = commonProject("macros", file("checkity-macros")).settings(
  libraryDependencies ++= Seq(scalaCompiler, scalaReflect)
).dependsOn(base)

lazy val core = commonProject("core", file("checkity-core")).dependsOn(base, macros)
