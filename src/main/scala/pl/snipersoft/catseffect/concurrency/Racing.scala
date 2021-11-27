package pl.snipersoft.catseffect.concurrency

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{FiberIO, IO, IOApp, OutcomeIO}
import pl.snipersoft.catseffect.utils.io.IoOps

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object Racing extends IOApp.Simple {

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
    (
      IO(s"Starting computation: $value").debug >>
        IO.sleep(duration) >> IO(s"Computation done: $value").debug >>
        IO(value)
      ).onCancel(IO(s"Computation cancelled: $value").debug.void)

  def testRace(): IO[String] = {
    val mol = runWithSleep(42, 1.second)
    val lang = runWithSleep("Scala", 2.seconds)

    val first: IO[Either[Int, String]] = IO.race(mol, lang)

    first.flatMap {
      case Left(n) => IO(s"Meaning of life: $n").debug
      case Right(s) => IO(s"Favourite language: $s").debug
    }
  }

  def testRacePair(): IO[Unit] = {
    val mol = runWithSleep(42, 1.second)
    val lang = runWithSleep("Scala", 2.seconds)

    val raceResult: IO[Either[
      (OutcomeIO[Int], FiberIO[String]), // (winner result, loser fiber)
      (FiberIO[Int], OutcomeIO[String]) // (loser fiber, winner result)
    ]] = IO.racePair(mol, lang)

    raceResult.flatMap {
      case Left((outputMol, fiberLang)) => fiberLang.cancel >> IO("MOL won").debug >> IO(outputMol).debug
      case Right((fiberMol, outputLang)) => fiberMol.cancel >> IO("Language won").debug >> IO(outputLang).debug
    }.void
  }

  override def run: IO[Unit] = testRacePair().void
}

object RacingExercise extends IOApp.Simple {

  import cats.syntax.either._

  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = IO.race(IO.sleep(duration), io).flatMap {
    case Left(_) => IO.raiseError(new RuntimeException("Timeout!"))
    case Right(value) => IO(value)
  }

  val io: IO[Int] = IO("Starting").debug >> IO.sleep(3.second) >> IO(35).debug
  val testTimeout: IO[Unit] = timeout(io, 2.seconds).void

  /** From Cats Effect */
  val testTimeoutV2: IO[Unit] = io.timeout(2.seconds).void

  /** Get loser */
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = IO.racePair(ioa, iob).flatMap {
    case Left((_, fiberB)) => fiberB.join.flatMap(ioFromOutcome).map(_.asRight)
    case Right((fiberA, _)) => fiberA.join.flatMap(ioFromOutcome).map(_.asLeft)
  }

  def ioFromOutcome[A](outcome: OutcomeIO[A]): IO[A] = outcome match {
    case Succeeded(fa) => fa
    case Errored(e) => IO.raiseError(e)
    case Canceled() => IO.raiseError(new RuntimeException("Canceled"))
  }

  val testUnrace: IO[Unit] = unrace(IO.sleep(2.seconds) >> IO("First"), IO.sleep(3.seconds) >> IO(43)).debug.void

  /** Implement race using racePair */
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = IO.racePair(ioa, iob).flatMap {
    case Left((outputA, fiberB)) => fiberB.cancel >> ioFromOutcome(outputA).map(_.asLeft) //can also implement: ioa canceled => return iob result
    case Right((fiberA, outputB)) => fiberA.cancel >> ioFromOutcome(outputB).map(_.asRight)
  }

  val testSimpleRace: IO[Unit] = simpleRace(IO.sleep(1.second) >> IO(2), IO.sleep(2.seconds) >> IO("Abc")).debug.void

  override def run: IO[Unit] =
//      testTimeout
//      testUnrace
      testSimpleRace
}