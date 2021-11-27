package pl.snipersoft.catseffect.utils

package object general {
  implicit class DebugWrapper[F[_], A](val fa: F[A]) extends AnyVal {
    import cats.Functor
    import cats.syntax.functor._

    def debug(implicit functor: Functor[F]): F[A] = fa.map { a =>
      val t = Thread.currentThread().getName
      println(s"[$t] $a")
      a
    }
  }
}
