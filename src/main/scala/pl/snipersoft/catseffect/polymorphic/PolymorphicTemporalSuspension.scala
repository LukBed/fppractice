package pl.snipersoft.catseffect.polymorphic

import cats.effect.IO
import cats.effect.kernel.{Concurrent, Temporal}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** Temporal describes time-blocking effects */
object PolymorphicTemporalSuspension {

  trait MyTemporal[F[_]] extends Concurrent[F] {
    def sleep(time: FiniteDuration): F[Unit] //semantically blocks the fiber
    def timeout[A](fa: F[A], duration: FiniteDuration): F[A]
  }

  val sleepIo: IO[Unit] = Temporal[IO].sleep(5.seconds)
  val timeoutIo: IO[String] = Temporal[IO].timeout(Temporal[IO].pure("Test"), 5.seconds)

  def timeout[F[_], A](fa: F[A], duration: FiniteDuration)(implicit temporal: Temporal[F]): F[A] = {
    import cats.syntax.flatMap._
    temporal.race(temporal.sleep(duration), fa).flatMap {
      case Left(_) => temporal.raiseError(new RuntimeException("Timeout!"))
      case Right(value) => temporal.pure(value)
    }
  }
}
