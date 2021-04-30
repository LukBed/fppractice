package pl.snipersoft.fppractice.numbers

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class LargestNumbersSpec extends AnyFunSuite with Matchers {
  val data = Table(
    ("input", "output"),
    (List(10, 2), "210"),
    (List(2, 10), "210"),
    (List(3, 30, 5, 9, 34), "9534330"),
    (List(), "0"))

  forAll(data) { (input: List[Int], output: String) =>
    test(s"should prepare largest number from $input") {
      LargestNumbers.prepare(input) shouldBe Some(output)
    }
  }

  test("should throw if list contains negative number") {
    LargestNumbers.prepare(List(2, -4)) shouldBe None
  }
}

class HelperSortingSpec extends AnyFunSuite with Matchers {
  val data = Table(
    ("weaker", "stronger"),
    (3, 4),
    (30, 4),
    (300, 4),
    (300, 3),
    (300, 30),
    (30, 310),
    (300, 31),
    (300, 0),
    (246, 32),
    (246, 310))

  forAll(data) { (weaker: Int, stronger: Int) =>
    LargestNumbers.isAStronger(weaker, stronger) shouldBe false
    LargestNumbers.isAStronger(stronger, weaker) shouldBe true
  }
}
