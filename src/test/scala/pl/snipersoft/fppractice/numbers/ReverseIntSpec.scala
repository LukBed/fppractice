package pl.snipersoft.fppractice.numbers

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import pl.snipersoft.fppractice.numbers.NumbersUtils.RichInt

class ReverseIntSpec extends AnyFunSuite with Matchers {
  val validData = Table(
    ("input", "expected"),
    (123, 321),
    (0, 0),
    (4567928, 8297654),
    (10, 1),
    (27800, 872),
    (-27800, -872),
    (-1, -1),
    (-10, -1)
  )

  val invalidData = Seq(2147483643, 1638454127, 1999999999, Int.MaxValue, Int.MinValue+1, Int.MinValue)

  forAll(validData) { (input: Int, expected: Int) =>
    test(s"should reverse $input") {
      input.reverseInt shouldBe Some(expected)
    }
  }

  invalidData.foreach { (input: Int) =>
    test(s"should not reverse $input") {
      input.reverseInt shouldBe None
    }
  }
}
