package pl.snipersoft.fppractice.lists

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class SortedTests extends AnyFunSuite with Matchers {
  val unsortedLists = Table(("nr", "list"),
    (1, 5 :: 4 :: 6 :: 1 :: 10 :: 3 :: 8 :: 9 :: 7 :: 2 :: MyNil),
    (2, 1 :: 3 :: 8 :: 2 :: 7 :: 6 :: 4 :: 5 :: 9 :: 10 :: MyNil))

  forAll(unsortedLists) { (n, l) =>
    test(s"should sort the list - case $n") {
      check(l)
    }
  }

  test("should sort the reversed list") {
    check(10 :: 9 :: 8 :: 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: 1 :: MyNil)
  }

  test("should sort the sorted list") {
    check(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: MyNil)
  }

  private def check(input: MyList[Int]) = {
    val expected = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: MyNil
    val sorted = input.sorted(Ordering.fromLessThan[Int](_ < _))
    sorted shouldBe expected
  }
}