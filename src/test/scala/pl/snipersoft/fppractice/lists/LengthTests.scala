package pl.snipersoft.fppractice.lists

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LengthTests extends AnyFunSuite with Matchers {
  test("should calculate correct length for multi-elements list") {
    val list = "a" :: "b" :: "c" :: MyNil
    list.length shouldBe 3
  }

  test("should calculate correct length for single element list") {
    val list = "a" :: MyNil
    list.length shouldBe 1
  }

  test("should return correct length for empty list") {
    MyNil.length shouldBe 0
  }
}