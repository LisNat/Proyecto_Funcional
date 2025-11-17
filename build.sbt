ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    name := "Proyecto_PFuncional",
    javacOptions ++= Seq("-source", "17", "-target", "17"),
    scalacOptions ++= Seq(
      "-language:implicitConversions",
      "-deprecation",
      "-feature",
      "-unchecked"
    ),
    libraryDependencies ++= Seq(
      "com.storm-enroute" %% "scalameter-core" % "0.21",
      "org.scalameta" %% "munit" % "0.7.26" % Test,
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
      "org.plotly-scala" %% "plotly-render" % "0.8.3"
    )
  )