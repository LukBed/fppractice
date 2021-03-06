package pl.snipersoft.fppractice.lists

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class MergeSortedSpec extends SortedSpec("merge sorting",
  (l: MyList[Int], o: Ordering[Int]) => l.mergeSorted(o))

class InsertionSortedSpec extends SortedSpec("insertion sorting",
  (l: MyList[Int], o: Ordering[Int]) => l.insertionSorted(o))

class QuickSortedSpec extends SortedSpec("quick sorting",
  (l: MyList[Int], o: Ordering[Int]) => l.quickSorted(o))

abstract class SortedSpec(val sortingType: String, val sort: (MyList[Int], Ordering[Int]) => MyList[Int])
  extends AnyFunSuite with Matchers {
  val ordering = Ordering.fromLessThan[Int](_ < _)
  val unsortedLists = Table(("nr", "list"),
    (1, 5 :: 4 :: 6 :: 1 :: 10 :: 3 :: 8 :: 9 :: 7 :: 2 :: MyNil),
    (2, 1 :: 3 :: 8 :: 2 :: 7 :: 6 :: 4 :: 5 :: 9 :: 10 :: MyNil))

  forAll(unsortedLists) { (n, l) =>
    test(s"should sort the list - case $n ($sortingType)") {
      check(l)
    }
  }

  test(s"should sort the reversed list ($sortingType)") {
    check(10 :: 9 :: 8 :: 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: 1 :: MyNil)
  }

  test(s"should sort the sorted list ($sortingType)") {
    check(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: MyNil)
  }

  test(s"should sort list with odd values ($sortingType)") {
    val list = 4 :: 7 :: 3 :: 2 :: 5 :: 6 :: 1 :: MyNil
    val expected = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: MyNil
    val sorted = sort(list, ordering)
    sorted shouldBe expected
  }

  test(s"should sort the single element list ($sortingType)") {
    val list = 1 :: MyNil
    val sorted = sort(list, ordering)
    sorted shouldBe list
  }

  private def check(input: MyList[Int]) = {
    val expected = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: MyNil
    val sorted = sort(input, ordering)
    sorted shouldBe expected
  }
}