package pl.snipersoft.fppractice.numbers

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PiApproximationSpec extends AnyFunSuite with Matchers {
  test("generated value should be close to pi") {
    PiApproximation(1000000) shouldBe (Math.PI +- 0.01)
  }
}
