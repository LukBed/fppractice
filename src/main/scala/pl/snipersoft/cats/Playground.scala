package pl.snipersoft.cats

import cats.Eval

object Playground extends App {
  val meaningOfLife = Eval.later {
    println("Computing meaning of life...")
    42
  }

  println(meaningOfLife.value)

  trait SequentialChecker[T[_]] {
    def isSequential: Boolean
  }
  val listChecker = new SequentialChecker[List] {
    override def isSequential: Boolean = true
  }
}
