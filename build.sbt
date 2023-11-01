ThisBuild / version := "0.2.0"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "dry",
    assembly / assemblyJarName := "dry-0.2.0.jar",
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.17",
      "org.scalatest" %% "scalatest" % "3.2.17" % "test",
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.17.0" % "test"
    )
  )

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")

lazy val createTests = taskKey[Unit]("Create tests for the Dry programming language")

createTests := DryTests.createMainTestFile()

lazy val testDry = taskKey[Unit]("Run tests for the Dry programming language")
testDry := {
  createTests.value
  (runMain in Compile).toTask(" com.melvic.dry.Main tests/tests.dry").value
}
