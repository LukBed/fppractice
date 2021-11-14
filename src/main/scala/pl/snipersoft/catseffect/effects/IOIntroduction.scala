package pl.snipersoft.catseffect.effects

import cats.effect.IO

import scala.io.StdIn.readLine

object IOIntroduction extends App {

  import cats.effect.unsafe.implicits.global //runtime context

  presentation()
  combineEffectsAsTuples()
  executeSmallProgram()

  def presentation(): Unit = {
    val pure: IO[Int] = IO.pure(42) //arg without side effects

    val delayed: IO[Int] = IO.delay {
      println("I'm producing an Integer")
      42
    }

    val delayedV2: IO[Int] = IO {
      println("I'm producing an Integer")
      42
    }

    val deferred = IO.defer(pure) // the same as IO.delayed(pure).flatten

    println(delayedV2.unsafeRunSync()) //end of the world
  }

  def executeSmallProgram(): Unit = {
    val smallProgram = for {
      firstLine <- IO(readLine())
      secondLine <- IO(readLine())
      total <- IO(firstLine + secondLine)
      _ <- IO(println(total))
    } yield ()

    smallProgram.unsafeRunSync()
  }

  def combineEffectsAsTuples(): Unit = {
    import cats.syntax.apply._
    val combined = (IO(42), IO(25)).mapN(_ + _)
    combined.map(println).unsafeRunSync()
  }
}

object IOExercises extends App {

  import cats.effect.unsafe.implicits.global

  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = ioa.flatMap(_ => iob)

  def sequenceTakeLastV2[A, B](ioa: IO[A], iob: IO[B]): IO[B] = ioa *> iob // andThen

  def sequenceTakeLastV3[A, B](ioa: IO[A], iob: IO[B]): IO[B] = ioa >> iob // andThen with by-name call (lazy evaluation)

  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = ioa.flatMap(a => iob.map(_ => a))

  def sequenceTakeFirstV2[A, B](ioa: IO[A], iob: IO[B]): IO[A] = ioa <* iob // before

  def forever[A](io: IO[A]): IO[A] = io.flatMap(a => forever(io))

  def forever2[A](io: IO[A]): IO[A] = io >> forever2(io)

  def forever3[A](io: IO[A]): IO[A] = io.foreverM

  def convert[A, B](ioa: IO[A], value: B): IO[B] = ioa.map(_ => value)

  def convert2[A, B](ioa: IO[A], value: B): IO[B] = ioa.as(value)

  def asUnit[A](ioa: IO[A]): IO[Unit] = ioa.map(_ => ())

  def asUnit2[A](ioa: IO[A]): IO[Unit] = ioa.as(()) // don't use it, hard to read

  def asUnit3[A](ioa: IO[A]): IO[Unit] = ioa.void

  //fixStackRecursion
  def sum(n: Int): Int = if (n <= 0) 0 else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] = if (n <= 0) IO(0) else for {
    last <- IO(n)
    next <- sumIO(n - 1)
  } yield last + next

  //  println(sum(100000)) // StackOverflowError
    sumIO(100000).map(println).unsafeRunSync() //705082704

  def safeFibonacci(n: Int): IO[BigInt] = {
    if (n < 2) IO(1)
    else for {
      _ <- IO(println(s"Calculating $n"))
      a <- IO(n - 1).flatMap(safeFibonacci)
      b <- IO(n - 2).flatMap(safeFibonacci)
    } yield a + b
  }

  (1 to 100).foreach(i => safeFibonacci(i).map(println).unsafeRunSync()
  )
}