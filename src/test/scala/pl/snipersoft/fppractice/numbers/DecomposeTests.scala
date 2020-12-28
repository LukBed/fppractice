package pl.snipersoft.fppractice.numbers

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import pl.snipersoft.fppractice.NumbersUtils

import NumbersUtils._

class DecomposeTests extends AnyFunSuite with Matchers {
  test("should decompose positive number") {
    25.decompose shouldBe List(1, 5, 25)
    1.decompose shouldBe List(1)
    8.decompose shouldBe List(1, 2, 4, 8)
    64.decompose shouldBe List(1, 2, 4, 8, 16, 32, 64)
    1000.decompose shouldBe List(1, 2, 4, 5, 8, 10, 20, 25, 40, 50, 100, 125, 200, 250, 500, 1000)
    367233.decompose shouldBe List(1, 3, 167, 501, 733, 2199, 122411, 367233)
  }

  test("should decompose negative number") {
    (-25).decompose shouldBe 25.decompose
    (-1).decompose shouldBe 1.decompose
    (-8).decompose shouldBe 8.decompose
    (-64).decompose shouldBe 64.decompose
    (-100).decompose shouldBe 100.decompose
    (-367233).decompose shouldBe 367233.decompose
  }

  test("zero should not have decomposited numbers") {
    0.decompose shouldBe Nil
  }

  test("prime number should have only two decomposited numbers") {
    val primes = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97,
      2003, 104711, 104717, 104723, 104729, 2731189)
    primes.foreach(n => n.decompose shouldBe List(1, n))
  }
}
