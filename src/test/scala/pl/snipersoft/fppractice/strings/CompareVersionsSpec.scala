package pl.snipersoft.fppractice.strings

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class CompareVersionsSpec extends AnyFunSuite with Matchers {
  val data = Table(
    ("a", "b", "expected"),
    ("0.9", "1", -1),
    ("1", "1.0.3", -1),
    ("1.0.3.4", "1.0.4.4", -1),
    ("1.0.3.4", "1.0.3.5", -1),
    ("1.0.8.4", "1.2.8.5", -1),
    ("1.0.3.4", "2.0", -1),
    ("2.0", "2.01", -1),
    ("2.0", "2.02", -1),
    ("1.1", "1.1", 0),
    ("1", "1.0", 0)
  )

  forAll(data) { (a, b, expected) => {
    test(s"comparing version '$a' with '$b' should be $expected") {
      CompareVersions.compare(a, b) shouldBe expected
    }
    test(s"comparing version '$b' with '$a' should be ${-expected} (opposite case)") {
      CompareVersions.compare(b, a) shouldBe (-expected)
    }
  }

  }



//a mniejsze -1
}
