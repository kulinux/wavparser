//val scalaVersion = "2.13.7"

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)

lazy val root = project
  .in(file("."))
  .settings(
    name := "wav",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.13.7",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.3.0",
      "org.scalactic" %% "scalactic" % "3.2.10" % "test",
      "org.scalatest" %% "scalatest" % "3.2.10" % "test"
    ),
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:postfixOps"
    )
  )
