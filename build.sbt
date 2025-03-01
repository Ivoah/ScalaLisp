ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.5"

lazy val root = (project in file("."))
  .settings(
    name := "ScalaLisp",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.0" % Test
  )
