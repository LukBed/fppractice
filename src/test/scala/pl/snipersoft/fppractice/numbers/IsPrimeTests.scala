package pl.snipersoft.fppractice.numbers

import org.scalatest.Inspectors.forAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import NumbersUtils._

class IsPrimeTests extends AnyFunSuite with Matchers {
  val primes = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97,
    2003, 104711, 104717, 104723, 104729, 2731189)
  val notPrimes = List(-100, -1, 0, 1, 4, 6, 12, 15, 18, 20, 32, 33, 34, 35, 36, 88, 99, 100, 104719, 367233)

  forAll(primes) { n =>
    test(s"number $n should be prime") {
      n.isPrime shouldBe true
    }
  }

  forAll(notPrimes) { n =>
    test(s"number $n should not be prime") {
      n.isPrime shouldBe false
    }
  }
}
