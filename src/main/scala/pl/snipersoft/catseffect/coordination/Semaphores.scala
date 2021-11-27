package pl.snipersoft.catseffect.coordination

import cats.effect.std.Semaphore
import cats.effect.{IO, IOApp}
import pl.snipersoft.catseffect.coordination.Semaphores.doWorkWhileLoggedId
import pl.snipersoft.catseffect.utils.IoOps

import scala.concurrent.duration.DurationInt
import scala.util.Random

/** Limit number of concurrent computation */
object Semaphores extends IOApp.Simple {

  val semaphore: IO[Semaphore[IO]] = Semaphore[IO](2) //2 total permits

  //limiting the number of concurrent sessions on a server
  def doWorkWhileLoggedId(): IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

  def logIn(id: Int, sem: Semaphore[IO]): IO[Int] = for {
    _ <- IO(s"Session $id: waiting to log in...").debug
    _ <- sem.acquire //waiting for permission, or sem.acquireN(1)
    _ <- IO(s"Session $id: logged in, working...").debug
    res <- doWorkWhileLoggedId()
    _ <- IO(s"Session $id: done '$res', logging out...'").debug
    _ <- sem.release // or sem.releaseN(1)
  } yield res

  def demo: IO[Unit] = for {
    sem <- Semaphore[IO](2)
    firstUserFib <- logIn(1, sem).start
    secondUserFib <- logIn(2, sem).start
    thirdUserFib <- logIn(3, sem).start
    fourthUserFib <- logIn(4, sem).start
    _ <- firstUserFib.join
    _ <- secondUserFib.join
    _ <- thirdUserFib.join
    _ <- fourthUserFib.join
  } yield ()


  override def run: IO[Unit] = demo
}

object SemaphoreExercise extends IOApp.Simple {
  import cats.syntax.parallel._

  val mutex: IO[Semaphore[IO]] = Semaphore[IO](1)

  def users(sem: Semaphore[IO]): IO[List[Int]] = (1 to 10).toList.parTraverse { id =>
    for {
      _ <- IO(s"Session $id: waiting to log in...").debug
      _ <- sem.acquire
      _ <- IO(s"Session $id: logged in, working...").debug
      res <- doWorkWhileLoggedId()
      _ <- IO(s"Session $id: done '$res', logging out...'").debug
      _ <- sem.release
    } yield res
  }

  override def run: IO[Unit] = mutex.flatMap(users).debug.void
}

