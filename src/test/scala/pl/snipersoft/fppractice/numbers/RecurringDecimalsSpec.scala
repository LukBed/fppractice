package pl.snipersoft.fppractice.numbers

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class RecurringDecimalsSpec extends AnyFunSuite with Matchers {
  val data = Table(
    ("numerator", "denominator", "result"),
    (1, 3, "0.(3)"),
    (-1, 3, "-0.(3)"),
    (1, -3, "-0.(3)"),
    (-1, -3, "0.(3)"),
    (1, 6, "0.1(6)"),
    (1, 2, "0.5"),
    (79, 6, "13.1(6)"),
    (-79, 6, "-13.1(6)"),
    (79, -6, "-13.1(6)"),
    (-79, -6, "13.1(6)"),
    (80, 2, "40"),
    (81, 2, "40.5"),
    (1, 333, "0.(003)"),
    (1, 7, "0.(142857)")
  )

  forAll(data) { (numerator: Int, denominator: Int, result: String) =>
    test(s"recurring decimals of $numerator/$denominator should be $result") {
      RecurringDecimals(numerator, denominator) shouldBe result
    }
  }

  test("should calculate recurring decimal of big prime number") {
    RecurringDecimals(1, 2003) should startWith("0.(00049925112")
  }
}
