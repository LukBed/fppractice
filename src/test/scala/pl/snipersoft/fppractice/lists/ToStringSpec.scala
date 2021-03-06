package pl.snipersoft.fppractice.lists

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ToStringSpec extends AnyFunSuite with Matchers {
  test("should generate correct toString for empty list") {
    MyNil.toString shouldBe "[]"
  }

  test("should generate correct toString for one element list") {
    (1 :: MyNil).toString shouldBe "[1]"
  }

  test("should generate correct toString for multiple elements list") {
    (1 :: 2 :: 3 :: MyNil).toString shouldBe "[1;2;3]"
  }
}
