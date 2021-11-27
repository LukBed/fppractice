package pl.snipersoft.catseffect.polymorphic

import cats.effect.kernel.Outcome
import cats.effect.{Fiber, IO, MonadCancel, Spawn}

/** Spawn creates fibers for any effect */
object PolymorphicSpawn {
  import cats.syntax.functor._
  import cats.syntax.flatMap._

  //all gen-prefixed type has generic error type
  trait MyGenSpawn[F[_], E] extends MonadCancel[F, Throwable] {
    def start[A](fa: F[A]): F[Fiber[F, E, A]] //creating a fiber effect
    def never[A]: F[A] //a forever-suspending effect
    def cede: F[Unit] //a "yield" effect, switch thread
  }

  trait MySpawn[F[_]] extends MyGenSpawn[F, Throwable]

//  import cats.effect.syntax.spawn._
//  Spawn[IO]

  def runOnSomeThread[F[_], A](fa: F[A])(implicit spawn: Spawn[F]): F[Outcome[F, Throwable, A]] = for {
      fib <- spawn.start(fa)
      outcome <- fib.join
    } yield outcome
}
