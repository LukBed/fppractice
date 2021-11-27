package pl.snipersoft.catseffect

import cats.effect.IO

import scala.concurrent.duration.DurationInt
import scala.util.Random

package object utils {
  implicit class IoOps[T](val io: IO[T]) {
    def debug: IO[T] = for {
      a <- io
      t = Thread.currentThread().getName
      _ <- IO(println(s"[$t] $a"))
    } yield a
  }

  def ioSleepMaxSecond: IO[Unit] = IO.sleep((Random.nextDouble() * 1000).toInt.millis)
}
