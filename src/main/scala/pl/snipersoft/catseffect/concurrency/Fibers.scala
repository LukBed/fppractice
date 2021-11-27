package pl.snipersoft.catseffect.concurrency

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, IO, IOApp}
import pl.snipersoft.catseffect.utils.io.IoOps

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object Fibers extends IOApp.Simple {

  val meaningOfLife: IO[Int] = IO.pure(42)
  val favouriteLanguage: IO[String] = IO.pure("Scala")

  //  def createFiber: Fiber[IO, Throwable, String] = ??? //almost impossible to create fiber manually

  //the fiber is not actually started, but the fiber allocation is wrapped in another effect (IO)
  val fiber: IO[Fiber[IO, Throwable, Int]] = meaningOfLife.debug.start

  def differentThreadIOs(): IO[Unit] = for {
    _ <- fiber //will be executed in another thread
    _ <- favouriteLanguage.debug
  } yield ()

  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    result <- fib.join //also an effect which waits fot the fiber to terminate
  } yield result
  /*
  Possible outcomes:
  -success with an IO
  -failure with an exception
  -cancelled
   */

  val someIOonAnotherThread: IO[Outcome[IO, Throwable, Int]] = runOnSomeOtherThread(meaningOfLife)
  val someResultFromAnotherThread: IO[Int] = someIOonAnotherThread.flatMap {
    case Succeeded(effect) => effect
    case Errored(_) => IO(0)
    case Canceled() => IO(-1)
  }

  def throwOnAnotherThread: IO[Outcome[IO, Throwable, Int]] = for {
    fib <- IO.raiseError[Int](new RuntimeException("Wrong number")).start
    result <- fib.join
  } yield result

  def testCancel: IO[Outcome[IO, Throwable, String]] = {
    val task = IO("Starting").debug >> IO.sleep(1.second) >> IO("Done").debug
    val withCancellationHandler = task.onCancel(IO("Canceled").debug.void) //good for set resources free

    for {
      fib <- withCancellationHandler.start //on a separate thread
      _ <- IO.sleep(500.millis) >> IO("Canceling") //on a calling thread
      _ <- fib.cancel
      result <- fib.join
    } yield result
  }

  override def run: IO[Unit] =
    runOnSomeOtherThread(meaningOfLife) //IO(Succeeded(IO(42)))
      //        testCancel //Canceled()
      //        throwOnAnotherThread //Errored(java.lang.RuntimeException: Wrong number
      .debug.void
}

object FiberExercises extends IOApp.Simple {
  def processResultFromFiber[A](io: IO[A]): IO[A] = io.start.flatMap(_.join).flatMap {
    case Succeeded(effect) => effect
    case Canceled() => IO.raiseError(new RuntimeException("Canceled"))
    case Errored(e) => IO.raiseError(e)
  }

  import cats.syntax.apply._

  def tupleIos[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = (for {
    fibA <- ioa.start
    fibB <- iob.start
    resA <- fibA.join
    resB <- fibB.join
  } yield (resA, resB)).flatMap {
    case (Succeeded(effA), Succeeded(effB)) => (effA, effB).tupled
    case (Errored(e), _) => IO.raiseError(e)
    case (_, Errored(e)) => IO.raiseError(e)
    case _ => IO.raiseError(new RuntimeException("Canceled"))
  }

  def timeout[A](ioa: IO[A], duration: FiniteDuration): IO[A] = (for {
    fibA <- ioa.start
    _ <- (IO.sleep(duration) >> fibA.cancel).start //careful - fibers can leak if applies external resources (ex. ports)
    resA <- fibA.join
  } yield resA).flatMap {
    case Succeeded(effect) => effect
    case Errored(e) => IO.raiseError(e)
    case Canceled() => IO.raiseError(new RuntimeException("Timeout!"))
  }

  override def run: IO[Unit] =
  //    processResultFromFiber(IO("Starting").debug >> IO.sleep(1.second) >> IO("Done").debug >> IO(42)).void
  //    tupleIos(IO.sleep(2.seconds) >> IO(42).debug, IO.sleep(3.seconds) >> IO("abc").debug).debug.void
    timeout(IO.sleep(2.seconds) >> IO("OK"), 1.second).debug.void //Timeout!
  //    timeout(IO.sleep(1.seconds) >> IO("OK"), 5.second).debug.void //OK
  //    timeout(IO.sleep(1.seconds) >> IO.raiseError(new RuntimeException("My error")), 5.second).debug.void //My error
}
