package pl.snipersoft.fppractice.lists

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RotateSpec extends AnyFunSuite with Matchers {
  val list = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: MyNil

  test("should rotate list") {
   val expected = 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: 1 :: 2 :: 3 :: MyNil
   list.rotate(3) shouldBe expected
 }

  test("should not rotate if rotate for n=0") {
    list.rotate(0) shouldBe list
  }

  test("should not rotate if rotate for n=length") {
    list.rotate(10) shouldBe list
  }

  test("should throw if rotate for negative n") {
    assertThrows[IllegalArgumentException] {
      list.rotate(-1) shouldBe list
    }
    assertThrows[IllegalArgumentException] {
      list.rotate(-5) shouldBe list
    }
  }

  test("should throw if rotate for n out of boundary") {
    assertThrows[IllegalArgumentException] {
      list.rotate(11) shouldBe list
    }
    assertThrows[IllegalArgumentException] {
      list.rotate(15) shouldBe list
    }
  }
}