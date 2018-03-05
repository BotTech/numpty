import sbt._

object Dependencies {

  object Versions {

    val scala = "2.12.3"

    val discipline = "0.8"
    val macroParadise = "2.1.0"
    val refined = "0.8.7"
    val scalaCheck = "1.13.5"
    val scalaTest = "3.0.4"
    val simulacrum = "0.12.0"
    val spire = "0.15.0"
  }

  val discipline = "org.typelevel" %% "discipline" % Versions.discipline
  val macroParadise = "org.scalamacros" % "paradise" % Versions.macroParadise
  val refined = "eu.timepit" %% "refined" % Versions.refined
  val scalaCheck = "org.scalacheck" %% "scalacheck" % Versions.scalaCheck
  val scalaTest = "org.scalatest" %% "scalatest" % Versions.scalaTest
  val simulacrum = "com.github.mpilquist" %% "simulacrum" % Versions.simulacrum
  val spire = "org.typelevel" %% "spire" % Versions.spire

  val testDependencies: Seq[ModuleID] = Seq(
    discipline,
    scalaCheck,
    scalaTest
  ).map(_.withConfigurations(Some(Test.name)))
}
