ThisBuild / organization := "net.ivoah"

ThisBuild / version := "0.2.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.3"

lazy val root = (project in file("."))
  .settings(
    name := "slippy",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.0" % Test
  )
