package pl.snipersoft.catseffect.coordination

import cats.effect.{IO, IOApp}
import pl.snipersoft.catseffect.utils.IoOps

import scala.concurrent.duration.DurationInt
import scala.util.Random

//implement Mutex class and object
abstract class Mutex {
  def acquire: IO[Unit]
  def release: IO[Unit]
}

object Mutex {
  def create: IO[Mutex] = ???
}

object MutexApp extends IOApp.Simple {
  import cats.syntax.parallel._

  def criticalTask(): IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

  def createNonLockingTask(id: Int): IO[Int] = for {
    _ <- IO(s"[task $id] working").debug
    res <- criticalTask()
    _ <- IO(s"[task $id] got result '$res'").debug
  } yield res

  def demoNonLockingTask(): IO[List[Int]] = (1 to 10).toList.parTraverse(createNonLockingTask)

  def createLockingTask(id: Int, mutex: Mutex): IO[Int] = for {
    _ <- IO(s"[task $id] waiting for permission").debug
    _ <- mutex.acquire //blocks if the mutex has been acquired by some other fiber
    _ <- IO(s"[task $id] working").debug
    res <- criticalTask()
    _ <- IO(s"[task $id] got result '$res'").debug
    _ <- mutex.release
    _ <- IO(s"[task $id] lock removed").debug
  } yield res

  def demoLockingTask(): IO[List[Int]] = for {
    mutex <- Mutex.create
    results <-  (1 to 10).toList.parTraverse(createLockingTask(_, mutex))
  } yield results

  override def run: IO[Unit] =
//    demoNonLockingTask().debug.void
    demoLockingTask().debug.void
}