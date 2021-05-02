package pl.snipersoft.fppractice.numbers

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import pl.snipersoft.fppractice.numbers.NumbersUtils.RichInt

class UglyNumbersSpec extends AnyFunSuite with Matchers {

  val firstUglyNumbers = Seq(1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15)
  val uglyNumbers = firstUglyNumbers ++ Seq(20, 25, 100, 1200, (2 * 2 * 3 * 5))
  val notUglyNumbers = Seq(-6, -1, 0, 7, 11, 13, 14, 28, 29, 456478, 7 * 2 * 2 * 3)

  uglyNumbers.foreach(n => {
    test(s"$n should be ugly number") {
      n.isUgly shouldBe true
    }
  })

  notUglyNumbers.foreach(n => {
    test(s"$n should not be ugly number") {
      n.isUgly shouldBe false
    }
  })

  Seq.iterate(0, firstUglyNumbers.size)(_ + 1).foreach { index =>
    test(s"should generate nth ugly number (${index + 1})") {
      NumbersUtils.nthUglyNumber(index + 1) shouldBe Some(firstUglyNumbers(index))
    }
  }

  test(s"should not generate nth ugly number for 0 index") {
    NumbersUtils.nthUglyNumber(0) shouldBe None
  }

  test(s"should not generate nth ugly number for negative index") {
    NumbersUtils.nthUglyNumber(-10) shouldBe None
    NumbersUtils.nthUglyNumber(-1) shouldBe None
  }


}
