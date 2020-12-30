package pl.snipersoft.fppractice.lists

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MyListReverseTests extends AnyFunSuite with Matchers {
  test("should reverse three-elements list") {
    val list = "a" :: "b" :: "c" :: MyNil
    val reversed = "c" :: "b" :: "a" :: MyNil
    list.reverse shouldBe reversed
  }

  test("should reverse multi-elements list") {
    val list = "a" :: "b" :: "c" :: "d" :: MyNil
    val reversed = "d" :: "c" :: "b" :: "a" :: MyNil
    list.reverse shouldBe reversed
  }

  test("should reverse single element list") {
    val list = "a" :: MyNil
    list.reverse shouldBe list
  }

  test("should reverse empty list") {
    MyNil.reverse shouldBe MyNil
  }
}
