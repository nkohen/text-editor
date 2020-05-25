name := "text-editor"

libraryDependencies ++= Deps.gui

mainClass := Some("TestText")

version := "0.1"

scalaVersion := "2.13.2"

maintainer := "nadavk25@gmail.com"

developers := List(
  Developer(
    "nkohen",
    "Nadav Kohen",
    "nadavk25@gmail.com",
    url("https://github.com/nkohen")
  )
)

enablePlugins(JavaAppPackaging)

cancelable in Global := true

// Fork a new JVM for 'run' and 'test:run' to avoid JavaFX double initialization problems
fork := true