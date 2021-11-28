package pl.snipersoft.catseffect.polymorphic

import cats.effect.{Deferred, Ref, Spawn}

/** Concurrent - Ref + Deferred for any effect type */
object PolymorphicCoordination {

  trait MyConcurrent[F[_]] extends Spawn[F] {
    def ref[A](a:A): F[Ref[F, A]]
    def deferred[A]: F[Deferred[F, A]]
  }
}
