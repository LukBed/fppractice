package pl.snipersoft.catseffect.effects

import cats.effect.{ExitCode, IO, IOApp}
import pl.snipersoft.catseffect.effects.Tools.program

import scala.io.StdIn.readLine

object Tools {
  val program: IO[Unit] = for {
    line <- IO(readLine())
    _ <- IO(println(line))
  } yield ()
}

object MyIOApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = program.as(ExitCode.Success)
}

object MySimpleIOApp extends IOApp.Simple {
  override def run: IO[Unit] = program
}