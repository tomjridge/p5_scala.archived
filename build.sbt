

lazy val root = (project in file(".")).
  settings(
    name := "p5_scala",
    version := "1.0",
    scalaVersion := "2.11.6",
    autoCompilerPlugins := true,

    mainClass in Compile := Some("p5.examples.P5_modular_parsing_example")

  )
