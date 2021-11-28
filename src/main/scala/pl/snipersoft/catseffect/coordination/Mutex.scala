package pl.snipersoft.catseffect.coordination

import cats.effect.{Concurrent, Deferred, IO, IOApp}
import pl.snipersoft.catseffect.utils.io.IoOps

import scala.collection.immutable.Queue
import scala.concurrent.duration.DurationInt
import scala.util.Random

//implement Mutex class and object
abstract class Mutex[F[_]] {
  def acquire: F[Unit]
  def release: F[Unit]
}

object Mutex {
  import cats.syntax.functor._
  import cats.syntax.flatMap._

  type Signal[F[_]] = Deferred[F, Unit]
  case class State[F[_]](locked: Boolean, waiting: Queue[Signal[F]])


  def create[F[_]](implicit concurrent: Concurrent[F]): F[Mutex[F]] = {


    val unlocked: State[F] = State(locked = false, Queue.empty)
    val unitF = concurrent.pure(())

    concurrent.ref(unlocked).map { state =>
      new Mutex[F] {
        override def acquire: F[Unit] = concurrent.deferred[Unit].flatMap { signal =>
          state.modify {
            case State(false, _) => State[F](locked = true, Queue.empty) -> unitF
            case State(true, queue) => State[F](locked = true, queue.enqueue(signal)) -> signal.get
          }
        }.flatten

        override def release: F[Unit] = state.modify {
          case State(false, _) => unlocked -> unitF
          case State(true, queue) if queue.isEmpty => unlocked -> unitF
          case State(true, queue) =>
            State(locked = true, queue.tail) -> (queue.head.complete(()) >> unitF)
        }.flatten
      }
    }
  }
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

  def createLockingTask(id: Int, mutex: Mutex[IO]): IO[Int] = for {
    _ <- IO(s"[task $id] waiting for permission").debug
    _ <- mutex.acquire //blocks if the mutex has been acquired by some other fiber
    _ <- IO(s"[task $id] working").debug
    res <- criticalTask()
    _ <- IO(s"[task $id] got result '$res'").debug
    _ <- mutex.release
    _ <- IO(s"[task $id] lock removed").debug
  } yield res

  def demoLockingTask(): IO[List[Int]] = for {
    mutex <- Mutex.create[IO]
    results <- (1 to 10).toList.parTraverse(createLockingTask(_, mutex))
  } yield results

  override def run: IO[Unit] =
  //    demoNonLockingTask().debug.void
    demoLockingTask().debug.void
}