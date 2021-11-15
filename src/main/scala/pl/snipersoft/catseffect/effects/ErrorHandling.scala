package pl.snipersoft.catseffect.effects

import cats.effect.IO

import scala.util.Try

object ErrorHandling extends App {

  import cats.effect.unsafe.implicits.global

  val failed: IO[Int] = IO.delay {
    throw new RuntimeException("Failed...")
  }

  //more readable
  val failure: IO[Int] = IO.raiseError(new RuntimeException("Failed too..."))

  val handled: IO[Int] = failure.handleErrorWith {
    case _: RuntimeException => IO(4)
  }

  val asEither: IO[Either[Throwable, Int]] = failure.attempt

  //transform a failure and the success in one go
  val redeemed = failure.redeem(ex => s"Failed by: '$ex''", value => s"It's OK: $value")
  val redeemedWith = failure.redeemWith(ex => IO(s"Failed by: '$ex''"), value => IO(s"It's OK: $value"))
}

object ErrorHandlingExercises extends App {
  def optionToIO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = option.fold(IO.raiseError[A](ifEmpty))(IO(_))

  def tryToIO[A](aTry: Try[A]): IO[A] = aTry.fold(IO.raiseError, IO.pure)

  def eitherToIo[A](either: Either[Throwable, A]): IO[A] = either.fold[IO[A]](IO.raiseError, IO(_))

  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] = io.redeem(handler, identity)

  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] = io.redeemWith(handler, IO(_))
}