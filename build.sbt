name := "fppractice"

version := "0.1"

scalaVersion := "2.13.4"

val scalaTestVersion = "3.2.3"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "org.scalatest" %% "scalatest-funsuite" % scalaTestVersion % Test
)