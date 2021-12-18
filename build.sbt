//val scalaVersion = "2.13.7"

lazy val root = project
  .in(file("."))
  .settings(
    name := "wav",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.13.7",
    libraryDependencies ++= Seq(
      "dev.optics" %% "monocle-core" % "3.0.0",
      "dev.optics" %% "monocle-macro" % "3.0.0",
      "org.typelevel" %% "cats-core" % "2.3.0-M1",
      "org.typelevel" %% "cats-effect" % "3.3.0",
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
