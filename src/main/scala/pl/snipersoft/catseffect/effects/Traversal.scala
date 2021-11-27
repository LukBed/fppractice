package pl.snipersoft.catseffect.effects

import cats.Traverse
import cats.effect.{IO, IOApp}
import pl.snipersoft.catseffect.utils.io.IoOps

object Traversal extends IOApp.Simple {
  def compute(s: String): IO[Int] = IO {
    Thread.sleep(1000)
    s.split(" ").length
  }.debug

  val workload = List("Hello there", "I love Scala", "Cats Effects is a piece of cake")

  import cats.Traverse
  import cats.instances.list._

  val listTraverse: Traverse[List] = Traverse[List]

  val ios: List[IO[Int]] = workload.map(compute)
  val sequenceIo: IO[List[Int]] = listTraverse.traverse(workload)(compute)

  import cats.syntax.parallel._

  val parallelIo: IO[List[Int]] = workload.parTraverse(compute)

  val sequenceIoV2: IO[List[Int]] =  Traverse[List].sequence(ios)
  val parallelIoV2: IO[List[Int]] = ios.parSequence //from parallel syntax

  override def run: IO[Unit] = parallelIo.map(_.sum).debug.void
}

object TraversalExercises extends IOApp.Simple {
  def sequenceGeneral[F[_] : Traverse, A](ios: F[IO[A]]): IO[F[A]] = {
    import cats.syntax.traverse._
    ios.traverse(identity)
  }

  def parallelSequenceGeneral[F[_] : Traverse, A](ios: F[IO[A]]): IO[F[A]] = {
    import cats.syntax.parallel._
    ios.parTraverse(identity)
  }


  override def run: IO[Unit] = IO.unit
}
