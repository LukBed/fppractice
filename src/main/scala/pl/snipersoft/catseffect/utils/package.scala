package pl.snipersoft.catseffect

import cats.effect.IO

package object utils {
  implicit class IoOps[T](val io: IO[T]) {
    def debug: IO[T] = for {
      a <- io
      t = Thread.currentThread().getName
      _ <- IO(println(s"[$t] $a"))
    } yield a
  }
}
