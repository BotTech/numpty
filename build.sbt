import Common._
import Dependencies._

lazy val root = commonProject("root", file(".")).settings(
  name := "numpty"
).aggregate(core)

lazy val core = commonProject("core", file("numpty-core"))
