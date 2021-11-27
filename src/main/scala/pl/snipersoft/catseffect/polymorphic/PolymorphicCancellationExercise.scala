package pl.snipersoft.catseffect.polymorphic

import cats.effect.{IO, IOApp, MonadCancel}
import pl.snipersoft.catseffect.utils.general.DebugWrapper

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object PolymorphicCancellationExercise extends IOApp.Simple {
  import cats.effect.syntax.monadCancel._
  import cats.syntax.functor._
  import cats.syntax.flatMap._

  def unsafeSleep[F[_], E](duration: FiniteDuration)(implicit mc: MonadCancel[F, E]): F[Unit] = mc.pure(Thread.sleep(duration.toMillis)) //not semantic blocking

  def createAuthFlow[F[_], E](implicit mc: MonadCancel[F, E]): F[Unit] = {
    val inputPassword: F[String] =
      mc.pure("Input password:").debug >> mc.pure("*****").debug >> unsafeSleep(2.seconds) >> mc.pure("MyPass").debug

    val verifyPassword: String => F[Boolean] = (pass: String) => mc.pure("Veryfing...").debug >> unsafeSleep(2.seconds) >> mc.pure(pass == "MyPass")

    mc.uncancelable { poll =>
      for {
        pass <- poll(inputPassword).onCancel(mc.pure("Authentication failed. Try again later.").debug >> mc.pure(())) //cancelable!
        verified <- verifyPassword(pass) //not cancelable
        _ <- if (verified) mc.pure("Hello!").debug else mc.pure("Wrong password!").debug //not cancelable
      } yield ()
    }
  }

  def program(implicit mc: MonadCancel[IO, Throwable]): IO[Unit] = for {
    authFib <- createAuthFlow[IO, Throwable].start
    _ <- unsafeSleep(3.second) >> authFib.cancel
  } yield ()

  override def run: IO[Unit] = program
}