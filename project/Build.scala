import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "com.manyangled",
    version := "0.0.1",
    scalaVersion := "2.11.5",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    scalacOptions ++= Seq()
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(
      run <<= run in Compile in core
    )
  ) aggregate(libs, macros, core)

  lazy val libs: Project = Project(
    "libs",
    file("libs"),
    settings = buildSettings ++ Seq(
      libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )

  lazy val macros: Project = Project(
    "macros",
    file("macros"),
    settings = buildSettings ++ Seq(
      libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  ) dependsOn(libs)

  lazy val core: Project = Project(
    "core",
    file("core"),
    settings = buildSettings
  ) dependsOn(libs, macros)
}
