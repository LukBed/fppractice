package pl.snipersoft.fppractice.numbers

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class DuplicatesSpec extends AnyFunSuite with Matchers {
  val data = Table(
    ("input", "expected"),
    (List(1, 1, 5), 5),
    (List(27, 100, 100, 10, 27, 3, 10), 3)
  )

  forAll(data) { (input, expected) =>
    test(s"should find not duplicated number in $input") {
        Duplicates.findNotDuplicatedNumber(input) shouldBe expected
    }
  }
}
