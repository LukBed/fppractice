package pl.snipersoft.catseffect.effects

import cats.Parallel
import cats.effect.{IO, IOApp}
import pl.snipersoft.catseffect.utils.io.IoOps

object Parallelism extends IOApp.Simple {

  import cats.syntax.apply._

  val meaningOfLife: IO[Int] = IO(42).debug
  val favouriteLanguage: IO[String] = IO("Scala").debug
  val failure: IO[String] = IO.raiseError(new RuntimeException("My error...")).debug
  val sequential: IO[String] = (meaningOfLife, favouriteLanguage).mapN((n, s) => s"My goal in life is $n and $s").debug

  val parallel1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife) //from Cats, not Cats Effect
  val parallel2: IO.Par[String] = Parallel[IO].parallel(favouriteLanguage)

  import cats.effect.implicits._

  val parallel: IO.Par[String] = (parallel1, parallel2).mapN((n, s) => s"My goal in life is $n and $s")
  val parallelFlatten: IO[String] = Parallel[IO].sequential(parallel)

  import cats.syntax.parallel._

  val parallelShorter: IO[String] = (meaningOfLife, favouriteLanguage).parMapN((n, s) => s"My goal in life is $n and $s")

  val parallelWithFailure: IO[String] = (meaningOfLife, failure).parMapN(_ + _)
  val anotherFailure: IO[String] = (failure, failure).parMapN(_+_) //we don't know which will fail first

  //  override def run: IO[Unit] = parallelFlatten.debug.void
  //  override def run: IO[Unit] = parallelShorter.debug.void
//  override def run: IO[Unit] = parallelWithFailure.debug.void
  override def run: IO[Unit] = anotherFailure.debug.void
}
