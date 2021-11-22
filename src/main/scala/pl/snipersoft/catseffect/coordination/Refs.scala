package pl.snipersoft.catseffect.coordination

import cats.effect.{IO, IOApp, Ref}
import cats.implicits.catsSyntaxTuple2Parallel
import pl.snipersoft.catseffect.utils.IoOps

import scala.concurrent.duration.DurationInt

/** Ref is a purely functional atomic reference */
object Refs extends IOApp.Simple {

  val atomicMol: IO[Ref[IO, Int]] = Ref[IO].of(42)
  val atomicMolV2: IO[Ref[IO, Int]] = IO.ref(42)

  val setMol: IO[Unit] = atomicMol.flatMap(_.set(43)) //thread-safe
  val getMol: IO[Int] = atomicMol.flatMap(_.get)
  val getOldValueAndSetMol: IO[Int] = atomicMol.flatMap(_.getAndSet(43))
  val increaseMol: IO[Unit] = atomicMol.flatMap(_.update(_ + 1))
  val increaseAndGetNewValue: IO[Int] = atomicMol.flatMap(_.updateAndGet(_ + 1))
  val increaseAndGetOldValue: IO[Int] = atomicMol.flatMap(_.getAndUpdate(_ + 1))
  val modifiedMol: IO[String] = atomicMol.flatMap(_.modify(n => (n + 1, s"New values is ${n + 1}")))

  val wordCounter: IO[Unit] = {
    def task(workload: String, total: Ref[IO, Int]): IO[Unit] = {
      //      total.update(_ + words)

      for {
        _ <- IO(s"Counting words for: '$workload'").debug
        words <- IO(workload.split(" ").length)
        newValue <- total.updateAndGet(_ + words)
        _ <- IO(s"New value is: '$newValue'").debug
      } yield ()
    }

    for {
      counter <- IO.ref(0)
      _ <- {
        import cats.syntax.parallel._
        List("Hello my friend, how are you?", "Star Wars is my favourite movie", "I like Avengers")
          .map(task(_, counter))
          .parSequence
      }
      _ <- counter.get.debug
    } yield ()
  }

  val exercise: IO[Unit] = {
    def tickingClock(ticks: Ref[IO, Int]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- ticks.update(_+1)
      _ <- tickingClock(ticks)
    } yield ()

    def printTicks(ticks: Ref[IO, Int]): IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      _ <- ticks.get.map(n => s"Ticks: '$n'").debug
      _ <- printTicks(ticks)
    } yield ()

    for {
      ticks <- IO.ref(0)
      _ <- (tickingClock(ticks), printTicks(ticks)).parTupled
    } yield ()
  }

  override def run: IO[Unit] =
//    wordCounter
    exercise
}
