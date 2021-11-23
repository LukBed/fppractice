package pl.snipersoft.catseffect.coordination

import cats.effect.{Deferred, Fiber, FiberIO, IO, IOApp, OutcomeIO, Ref}
import pl.snipersoft.catseffect.utils.IoOps

import scala.concurrent.duration.DurationInt

/**
 * Defer is a primitive for waiting for an effect, while some other effects completes with a value.
 * Very similar to Promise.
 */
object Defers extends IOApp.Simple {

  val deferred: IO[Deferred[IO, Int]] = Deferred[IO, Int]
  val deferredV2: IO[Deferred[IO, Int]] = IO.deferred[Int]
  val valueFromDeferred: IO[Int] = deferred.flatMap(_.get) //get blocks fiber semantically until the value is completed
  val completedDeferred: IO[Boolean] = deferred.flatMap(_.complete(42))

  val producerConsumer: IO[Unit] = {
    def consumer(signal: Deferred[IO, Int]): IO[Unit] = for {
      _ <- IO("[Consumer]: waiting for result").debug
      mol <- signal.get //blocker
      _ <- IO(s"[Consumer]: got the result '$mol'").debug
    } yield ()

    def producer(signal: Deferred[IO, Int]): IO[Unit] = for {
      _ <- IO("[Producer]: calculating...").debug
      _ <- IO.sleep(1.second)
      mol <- IO("[Producer]: completed").debug >> IO(42)
      _ <- signal.complete(mol)
    } yield ()

    for {
      signal <- IO.deferred[Int]
      fibProducer <- producer(signal).start
      fibConsumer <- consumer(signal).start
      _ <- fibProducer.join
      _ <- fibConsumer.join
    } yield ()
  }

  val downloading: IO[Unit] = {
    import cats.syntax.traverse._

    val fileParts = List("I ", "love ", "Scala ", "with ", "Cats ", "Effect!<EOF>")

    def downloadFile(contentRef: Ref[IO, String], fileDeferred: Deferred[IO, String]): IO[Unit] =
      fileParts.map { part => IO(s"Got '$part'").debug >> contentRef.updateAndGet(_ + part).flatMap(notifyIfCompleted(_, fileDeferred)) }.sequence.void

    def notifyIfCompleted(file: String, fileDeferred: Deferred[IO, String]): IO[Unit] =
      if (file.contains("<EOF>")) fileDeferred.complete(file).void else IO.sleep(1.second)

    for {
      fileDeferred <- IO.deferred[String]
      contentRef <- IO.ref("")
      _ <- downloadFile(contentRef, fileDeferred).start
      fileFib <- fileDeferred.get.map(s => s"Downloaded file: $s").debug.start
      _ <- fileFib.join
    } yield ()
  }

  override def run: IO[Unit] =
  //    producerConsumer
    downloading
}

object DefersAlarmExercise extends IOApp.Simple {

  def incrementer(counter: Ref[IO, Int], alarm: Deferred[IO, String]): IO[Unit] = IO.sleep(1.second) >> increaseAndNotify(counter, alarm)

  def increaseAndNotify(counter: Ref[IO, Int], alarm: Deferred[IO, String]): IO[Unit] = for {
    n <- counter.updateAndGet(_ + 1).debug
    _ <- if (n >= 10) alarm.complete("Time's up!") else incrementer(counter, alarm)
  } yield ()

  override def run: IO[Unit] = for {
    counter <- IO.ref(0)
    alarm <- IO.deferred[String]
    _ <- incrementer(counter, alarm)
    _ <- alarm.get.debug
  } yield ()
}

object Challenge extends IOApp.Simple {
  type RaceResult[A, B] = Either[
    (OutcomeIO[A], FiberIO[B]), // (winner result, loser fiber)
    (FiberIO[A], OutcomeIO[B]) // (loser fiber, winner result)
  ]

  type EitherOutcome[A, B] = Either[OutcomeIO[A], OutcomeIO[B]]

  def racePairWithDeferred[A, B](ioa: IO[A], iob: IO[B]): IO[RaceResult[A, B]] = IO.uncancelable { poll =>
    for {
      signal <- IO.deferred[EitherOutcome[A, B]]
      fibA <- ioa.guaranteeCase(outcomeA => signal.complete(Left(outcomeA)).void).start
      fibB <- iob.guaranteeCase(outcomeB => signal.complete(Right(outcomeB)).void).start
      result <- poll(signal.get).onCancel {
        for {
          cancelA <- fibA.cancel.start
          cancelB <- fibB.cancel.start
          _ <- cancelA.join
          _ <- cancelB.join
        } yield ()
      }
    } yield result match {
      case Left(outcomeA) => Left((outcomeA, fibB))
      case Right(outcomeB) => Right((fibA, outcomeB))
    }
  }

  override def run: IO[Unit] = racePairWithDeferred(IO.sleep(1.second) >> IO("Hello").debug, IO.sleep(500.millis) >> IO(42).debug).void
}