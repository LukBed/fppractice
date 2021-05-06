package pl.snipersoft.scala_api

object Sorting extends App {

  numbers()
  wrappedNumbers()

  def numbers(): Unit = {
    val numbers = List(2, 3, 1, 4)
    val expectedFromLess = List(1, 2, 3, 4)
    val expectedFromGreater = List(4, 3, 2, 1)

    assert(numbers.sorted == expectedFromLess)
    assert(numbers.sorted(Ordering.Int.reverse) == expectedFromGreater)
  }

  def wrappedNumbers(): Unit = {
    case class C(n: Int)

    val numbers = List(C(2), C(3), C(1), C(4))
    val expectedFromLess = List(C(1), C(2), C(3), C(4))
    val expectedFromGreater = List(C(4), C(3), C(2), C(1))

    val cOrdering = Ordering.fromLessThan((o1: C, o2: C) => o1.n < o2.n) //or implicit


    val fromLess = numbers.sorted(cOrdering)
    val fromGreater = numbers.sorted(cOrdering.reverse)

    println(fromLess)
    println(fromGreater)

    assert(fromLess == expectedFromLess)
    assert(fromGreater == expectedFromGreater)
  }
}
