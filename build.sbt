ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "dry"
  )

lazy val createTests = taskKey[Unit]("Create tests for the Dry programming language")

createTests := DryTests.createMainTestFile

lazy val testDry = taskKey[Unit]("Run tests for the Dry programming language")
testDry := {
  createTests.value
  (runMain in Compile).toTask(" com.melvic.dry.Main tests/test.dry").value
}
