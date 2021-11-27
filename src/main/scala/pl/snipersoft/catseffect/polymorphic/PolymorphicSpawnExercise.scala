package pl.snipersoft.catseffect.polymorphic

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, IOApp, Outcome, Spawn}

object PolymorphicSpawnExercise extends IOApp.Simple {

  def simpleRace[F[_], A, B](ioa: F[A], iob: F[B])(implicit spawn: Spawn[F]): F[Either[A, B]] = {
    import cats.syntax.functor._
    import cats.syntax.flatMap._
    import cats.syntax.either._

    def ioFromOutcome[T](outcome: Outcome[F, Throwable, T]): F[T] = outcome match {
      case Succeeded(fa) => fa
      case Errored(e) => spawn.raiseError(e)
      case Canceled() => spawn.raiseError(new RuntimeException("Canceled"))
    }

    spawn.racePair(ioa, iob).flatMap {
      case Left((outputA, fiberB)) => fiberB.cancel >> ioFromOutcome(outputA).map(_.asLeft[B])
      case Right((fiberA, outputB)) => fiberA.cancel >> ioFromOutcome(outputB).map(_.asRight[A])
    }
  }

  override def run: IO[Unit] = IO.unit
}
