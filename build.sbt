//val scalaVersion = "2.13.7"

lazy val root = project
  .in(file("."))
  .settings(
    name := "wav",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.13.7",
    libraryDependencies ++= Seq(
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
