import Common._
import Dependencies._
import sbt._

lazy val root = commonProject("root", file(".")).settings(
  name := "numpty"
).aggregate(
  core,
  testkit,
  `core-test`
)

lazy val core = commonProject("core", file("numpty-core"))

lazy val testkit = commonProject("testkit", file("numpty-testkit"))
  .dependsOn(core)
  .settings(
    libraryDependencies += scalaCheck
  )

lazy val `core-test` = commonProject("core-test", file("numpty-core-test"))
  .dependsOn(testkit)
