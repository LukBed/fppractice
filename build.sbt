name := "fppractice"

version := "0.1"

scalaVersion := "2.13.4"

val scalaTestVersion = "3.2.3"
val catsVersion = "2.6.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "org.scalatest" %% "scalatest-funsuite" % scalaTestVersion % Test
)

scalacOptions ++=Seq("-language:higherKinds")