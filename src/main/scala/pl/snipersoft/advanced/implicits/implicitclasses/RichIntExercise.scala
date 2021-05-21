package pl.snipersoft.advanced.implicits.implicitclasses

import scala.annotation.tailrec

object RichIntExercise extends App {

  implicit class RichInt(val n: Int) extends AnyVal {
    def times(f: Int => Unit): Unit = {
      @tailrec
      def tailRec(i: Int): Unit = {
        if (i > 0) {
          f(n)
          tailRec(i - 1)
        }
      }

      tailRec(n)
    }

    def *[T](l: Seq[T]): Seq[T] = (1 to n).flatMap(_ => l).toList
  }

  3.times(println)
  println(3 * List("a", "b"))
}
