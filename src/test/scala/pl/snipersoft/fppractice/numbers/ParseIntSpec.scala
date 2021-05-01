package pl.snipersoft.fppractice.numbers

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class ParseIntSpec extends AnyFunSuite with Matchers {
  val validData = Table(
    ("input", "expected"),
    ("123", 123),
    ("4587", 4587),
    ("-4587", -4587),
    ("  -123", -123),
    ("  +123", 123),
    ("  123", 123),
    ("789", 789),
    ("+789", 789),
    ("+789,213", 789),
    ("+789a213", 789),
    ("789 213", 789),
    (" 0", 0),
    (" -0", 0),
    ("gfhdf", 0),
    ("", 0),
    (Int.MaxValue.toString, Int.MaxValue),
    (Int.MinValue.toString, Int.MinValue),
    ((Int.MaxValue - 1).toString, Int.MaxValue - 1),
    ((Int.MinValue + 1).toString, Int.MinValue + 1),
  )

  val invalidData = Table(
    "value", Long.MaxValue.toString, Long.MinValue.toString, (Int.MaxValue.toLong+1).toString, (Int.MinValue.toLong-1).toString
  )

  forAll(validData) { (input: String, expected: Int) =>
    test(s"should parse '$input' as $expected") {
      NumbersUtils.parseInt(input) shouldBe Some(expected)
    }
  }

  forAll(invalidData) { (value: String) =>
    test(s"should not parse '$value'") {
      NumbersUtils.parseInt(value) shouldBe None
    }
  }

}
