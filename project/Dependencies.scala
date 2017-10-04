import sbt._

object Dependencies {

  object Versions {

    val scalaVersion = "2.12.3"

    val scalaCheckVersion = "1.13.5"
    val scalaTestVersion = "3.0.4"
  }

  import Versions._

  val scalaCompiler = "org.scala-lang" % "scala-compiler" % scalaVersion
  val scalaReflect = "org.scala-lang" % "scala-reflect" % scalaVersion

  val scalaCheck = "org.scalacheck" %% "scalacheck" % scalaCheckVersion
  val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion

  val compileDependencies = Seq(
    scalaCheck
  )

  val testDependencies = Seq(
    scalaTest
  ).map(_.withConfigurations(Some(Test.name)))
}
