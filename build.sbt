ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2021",
    idePackagePrefix := Some("io.github.mladensavic94")
  )
