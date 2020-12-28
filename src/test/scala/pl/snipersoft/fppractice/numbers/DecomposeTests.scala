package pl.snipersoft.fppractice.numbers

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import pl.snipersoft.fppractice.Numbers

class DecomposeTests extends AnyFunSuite with Matchers {
  test("should decompose positive number") {
    Numbers.decompose(25) shouldBe List(1, 5, 25)
    Numbers.decompose(1) shouldBe List(1)
    Numbers.decompose(8) shouldBe List(1, 2, 4, 8)
    Numbers.decompose(64) shouldBe List(1, 2, 4, 8, 16, 32, 64)
    Numbers.decompose(1000) shouldBe List(1, 2, 4, 5, 8, 10, 20, 25, 40, 50, 100, 125, 200, 250, 500, 1000)
    Numbers.decompose(367233) shouldBe List(1, 3, 167, 501, 733, 2199, 122411, 367233)
  }

  test("should decompose negative number") {
    Numbers.decompose(-25) shouldBe Numbers.decompose(25)
    Numbers.decompose(-1) shouldBe Numbers.decompose(1)
    Numbers.decompose(-8) shouldBe Numbers.decompose(8)
    Numbers.decompose(-64) shouldBe Numbers.decompose(64)
    Numbers.decompose(-100) shouldBe Numbers.decompose(100)
    Numbers.decompose(-367233) shouldBe Numbers.decompose(367233)
  }

  test("zero should not have decomposited numbers") {
    Numbers.decompose(0) shouldBe Nil
  }

  test("prime number should have only two decomposited numbers") {
    val primes = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97,
      2003, 104711, 104717, 104723, 104729, 2731189)
    primes.foreach(n => Numbers.decompose(n) shouldBe List(1, n))
  }
}
