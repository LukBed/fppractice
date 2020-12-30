package pl.snipersoft.fppractice.lists

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MyListGetElement extends AnyFunSuite with Matchers {
  test("should get right element from multi-elements list") {
    val list = "a" :: "b" :: "c" :: MyNil
    list(0) shouldBe "a"
    list(1) shouldBe "b"
    list(2) shouldBe "c"
  }

  test("should get right element from single element list") {
    val list = "a" :: MyNil
    list(0) shouldBe "a"
  }

  test("should thrown exception if get element out of boundary") {
    val list = "a" :: "b" :: "c" :: MyNil
    assertThrows[NoSuchElementException] {
      list(3)
    }

    assertThrows[NoSuchElementException] {
      list(-1)
    }
  }

  test("should thrown exception if get element from empty list") {
    assertThrows[NoSuchElementException] {
      MyNil(0)
      MyNil(1)
    }
  }

}
