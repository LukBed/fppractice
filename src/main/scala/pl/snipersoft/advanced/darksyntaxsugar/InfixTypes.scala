package pl.snipersoft.advanced.darksyntaxsugar

object InfixTypes extends App {
  class Composite[A, B]
  val firstComposite: Composite[Int, String] = ???
  val secondComposite: Int Composite String = ???

  class -->[A, B]
  val towards: Int --> String = ???
}