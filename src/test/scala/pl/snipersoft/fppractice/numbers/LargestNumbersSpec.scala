package pl.snipersoft.fppractice.numbers

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class LargestNumbersSpec extends AnyFunSuite with Matchers {
  val data = Table(
    ("input", "output"),
    (List(10, 2), "210"),
    (List(2, 10), "210"),
    (List(2), "2"),
    (List(0), "0"),
    (List(0, 0, 0), "0"),
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
