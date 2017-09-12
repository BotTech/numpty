import sbt._

object Dependencies {

  object Versions {

    val scalaVersion = "2.12.3"

    val scalaCheckVersion = "1.13.5"
  }

  import Versions._

  val scalaCheck = "org.scalacheck" %% "scalacheck" % scalaCheckVersion

  val compileDependencies = Seq(
    scalaCheck
  )
}
