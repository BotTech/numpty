import sbt._

object Dependencies {

  object Versions {

    val scala = "2.12.3"

    val macroParadise = "2.1.0"

    val simulacrum = "0.11.0"

    val scalaCheck = "1.13.5"
    val scalaTest = "3.0.4"

    val discipline = "0.8"
  }

  val macroParadise = "org.scalamacros" % "paradise" % Versions.macroParadise

  val simulacrum = "com.github.mpilquist" %% "simulacrum" % Versions.simulacrum

  val scalaCheck = "org.scalacheck" %% "scalacheck" % Versions.scalaCheck
  val scalaTest = "org.scalatest" %% "scalatest" % Versions.scalaTest

  val discipline = "org.typelevel" %% "discipline" % Versions.discipline

  val testDependencies: Seq[ModuleID] = Seq(
    scalaCheck,
    scalaTest,
    discipline
  ).map(_.withConfigurations(Some(Test.name)))
}
