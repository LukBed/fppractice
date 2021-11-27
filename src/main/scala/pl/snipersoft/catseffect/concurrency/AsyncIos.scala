package pl.snipersoft.catseffect.concurrency

import cats.effect.{IO, IOApp}
import pl.snipersoft.catseffect.utils.io.IoOps

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.concurrent.duration.DurationInt

/** IOs can be run asynchronously on fibers without having to manually manage the fiber lifecycle */
object AsyncIos extends IOApp.Simple {

  val threadPool: ExecutorService = Executors.newFixedThreadPool(8)
  val ec: ExecutionContext = ExecutionContext.fromExecutorService(threadPool)
  type Callback[A] = Either[Throwable, A] => Unit

  def computeMol(): Either[Throwable, Int] = Try {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] is computing meaning of life")
    42
  }.toEither

  def computeMolOnThreadPool(): Unit = threadPool.execute(() => computeMol()) //return Unit :(

  //async is a Foreign Function Interface FFI
  val asyncMol: IO[Int] = IO.async_ { callback =>
    threadPool.execute { () => //CE thread blocks (semantically) until this callback is invoked by some other thread
      val result = computeMol()
      callback(result) //CE thread is notified with the result
    }
  }

  //exercise - generalization
  def asyncToIo[A](computation: () => A)(ec: ExecutionContext): IO[A] = IO.async_ { callback =>
    ec.execute { () =>
      val result = Try(computation()).toEither
      callback(result)
    }
  }

  //exercise
  lazy val molFuture: Future[Int] = Future {
    Thread.sleep(1000); 42
  } (ec)
  def ioFromFuture[A](future: => Future[A])(implicit ec: ExecutionContext): IO[A] = IO.async_ { callback =>
    future.onComplete { res => callback(res.toEither) }
  }
  def ioFromFutureV2[A](future: => Future[A])(implicit ec: ExecutionContext): IO[A] = IO.fromFuture(IO(future))

  //exercise
  val neverEndingIo: IO[Int] = IO.async_(_ => ()) //no callback, not finish
  val neverEndingIoV2: IO[Int] = IO.never

  //async give more control - after cancelation too
  def fullAsync(): IO[Unit] = {
    val fullAsyncIo: IO[Int] = IO.async { callback =>
      //return IO[Option[IO[Unit]]
      //finalizers of type IO[Unit] - called after cancelation
      //no finalizer - IO[None]
      IO {
        threadPool.execute { () =>
          val result = computeMol()
          callback(result)
        }
      }.as(Some(IO("Cancelled!").debug.void)) //or just map
    }

    for {
      fib <- fullAsyncIo.start
      _ <- IO.sleep(500.millis) >> IO("Canceling...").debug >> fib.cancel
      _ <- fib.join
    } yield ()
  }

  override def run: IO[Unit] =
  //    asyncMol.debug >> IO(threadPool.shutdown())
  //    asyncToIo(() => { Thread.sleep(1000); 32})(ec).debug.void >> IO(threadPool.shutdown())
  //    ioFromFuture(molFuture)(ec).debug.void >> IO(threadPool.shutdown())
  //    neverEndingIo.void
    fullAsync()
}
