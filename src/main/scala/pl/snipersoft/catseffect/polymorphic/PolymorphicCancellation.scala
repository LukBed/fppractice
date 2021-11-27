package pl.snipersoft.catseffect.polymorphic

import cats.effect.Poll
import cats.{Applicative, Monad}

object PolymorphicCancellation {

  trait MyApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](error: E): F[A]
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
  }

  trait MyMonadError [F[_], E] extends MyApplicativeError[F, E] with Monad[F]

  trait MyPoll[F[_]] {
    def apply[A](fa: F[A]): F[A]
  }

  trait MyMonadCancel[F[_], E] extends MyMonadError[F, E] {
    def cancel: F[Unit]
    def uncancellable[A](poll: Poll[F] => F[A]): F[A]
  }

  /*
  import cats.effect.syntax.monadCancel._
  import cats.effect.MonadCancel
  MonadCancel[IO].onCancel() //cancellation listener
  MonadCancel[IO].guaranteeCase() //finalizers
  MonadCancel[IO].bracket() //bracket pattern for releasing resources
   */
}


