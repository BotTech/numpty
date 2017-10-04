import Dependencies._
import sbt.Keys._
import sbt._
import sbt.io.syntax.File
import sbt.librarymanagement.Resolver

object Common {

  val commonSettings = Seq(
    organization := "nz.co.bottech",
    organizationName := "BotTech",
    version := "0.1",
    scalaVersion := Versions.scalaVersion,
    crossScalaVersions := Seq(scalaVersion.value),
    scalacOptions += "-feature",
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= compileDependencies ++ testDependencies
  )

  def commonProject(id: String, file: File): Project = Project(id, file).settings(commonSettings)
}
