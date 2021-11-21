package pl.snipersoft.catseffect.concurrency

import cats.effect.{IO, IOApp}
import pl.snipersoft.catseffect.utils.IoOps

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

/** Blocking calls and semantic blocking (IO.sleep) yield control over the calling thread automatically */
object Blocking extends IOApp.Simple {
  //different threads, so not really blocking
  val semanticBlocking: IO[Unit] = for {
    _ <- IO.sleep(1.second).debug //first thread
    _ <- IO.sleep(1.second).debug //second thread
  } yield ()

  val reallyBlocking: IO[Unit] = IO.blocking { //will be executed by dedicated thread pool
    Thread.sleep(1000)
    println(s"[${Thread.currentThread()}] computed a blocking code")
  }

  //yielding
  val ioOnManyThreads: IO[Unit] = for {
    _ <- IO("First").debug
    _ <- IO.cede //a signal to yield control over the thread, IO.shift from CE2 - next computation will be executed on different thread
    _ <- IO("Second").debug
    _ <- IO.cede //without cede all computations will be executed on the same thread sequential
    _ <- IO("Thread").debug
  } yield ()
  //we won't see different threads here due to CA runtime optimisation

  def presentIoOnManyThreads: IO[Int] = {
    //use different execution context to avoid CE optimisation
    val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    (1 to 1000).map(IO.pure).reduce(_.debug >> IO.cede >> _.debug).evalOn(ec)
  }

  override def run: IO[Unit] =
  //    semanticBlocking
  //    reallyBlocking
  //    ioOnManyThreads
    presentIoOnManyThreads.void
}
