package pl.snipersoft.catseffect.coordination

import cats.effect.kernel.Deferred
import cats.effect.std.CyclicBarrier
import cats.effect.{IO, IOApp}
import pl.snipersoft.catseffect.utils.io.{IoOps, ioSleepMaxSecond}

/**
 * For batching.
 * This is a coordination primitive that is initialized with a count and has a single method: await.
 * Barrier will semantically blocks all fibers calling its await method, until we have exactly n fibers waiting,
 * at which point the barrier will unblock all fibers and reset to its original state.
 * Any further fiber will again block until we have exactly n fibers waiting.
 * And so on.
 */
object CyclingBarriers extends IOApp.Simple {

  import cats.syntax.parallel._

  def createUser(id: Int, barrier: CyclicBarrier[IO]): IO[Unit] =
    ioSleepMaxSecond >>
      IO(s"[user $id] Waiting for signing up...").debug >>
      ioSleepMaxSecond >>
      IO(s"[user $id] On the wait list...").debug >>
      barrier.await >>
      IO(s"[user $id] I'm in...").debug >>
      IO.unit

  def openNetwork(): IO[Unit] = for {
    _ <- IO("Waiting for ten 10 users...").debug
    barrier <- CyclicBarrier[IO](10)
    _ <- (1 to 14).toList.parTraverse(createUser(_, barrier))
  } yield ()

  override def run: IO[Unit] = openNetwork()
}

object MyCyclingBarriersExercise {
  //ignoring cancellation
  abstract class MyCyclingBarrier {
    def await(): IO[Unit]
  }

  object MyCyclingBarrier {
    private case class State(remaining: Int, signal: Deferred[IO, Unit])

    private def initialState(n: Int): IO[State] = IO.deferred[Unit].map(signal => State(n - 1, signal))

    def apply(n: Int): IO[MyCyclingBarrier] = for {
      state <- initialState(n).flatMap(IO.ref)
    } yield new MyCyclingBarrier {
      override def await(): IO[Unit] = initialState(n).flatMap { newInitialState =>
        state.modify {
          case State(0, signal) => (newInitialState, signal.complete(()).void)
          case State(x, signal) => (State(x - 1, signal), signal.get)
        }.flatten
      }
    }
  }
}