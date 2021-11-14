package pl.snipersoft.catseffect.effects

import java.time.Instant

/*
Effect types:
- type signature describes the kind of calculation that will be performed
- type signature describes the value that will be calculated
- when side effects are needed, effect construction is separate from effect execution
Option is an effect type, future not
 */

/**
 * MyIO is an effect type:
 * -describes any computation that might produce side effects
 * -calculates value of type A, if it's successful
 * -side effects are required for the evaluation of () => A,
 * creation of MyIO does not produce side effects on construction
 */
case class MyIO[A](unsafeRun: () => A) {
  def map[B](f: A => B): MyIO[B] = MyIO(() => f(unsafeRun()))

  def flatMap[B](f: A => MyIO[B]): MyIO[B] = MyIO(() => f(unsafeRun()).unsafeRun())
}

object EffectsExercise extends App {
  measureTimeOfComputation()
  readAndPrint()

  def measureTimeOfComputation(): Unit = {
    lazy val timeIO = MyIO(() => Instant.now().toEpochMilli)

    /** Mls of computation */
    def measure[A](computation: MyIO[A]): MyIO[Long] = timeIO.flatMap(start => {
      computation.flatMap(_ => timeIO).map(_ - start)
    })

    val myIO = MyIO(() => {
      Thread.sleep(150)
      42
    })
    val measureIO = measure(myIO)

    println(measureIO.unsafeRun())
    println(measureIO.unsafeRun())
    println(measureIO.unsafeRun())
  }

  def readAndPrint(): Unit = {
    import scala.io.StdIn.readLine
    val readIo: MyIO[String] = MyIO(() => readLine())
    def printIo(s: String): MyIO[Unit] = MyIO(() => println(s))

    val myIo = for {
      _ <- printIo("First name: ")
      firstName <- readIo
      _ <- printIo("Last name: ")
      lastName <- readIo
      _ <- printIo(s"Hello, $firstName $lastName")
    } yield ()

    myIo.unsafeRun()
    myIo.unsafeRun()
  }
}