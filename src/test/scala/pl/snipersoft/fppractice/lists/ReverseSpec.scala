package pl.snipersoft.fppractice.lists

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ReverseSpec extends AnyFunSuite with Matchers {
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

  test("should reverse big list") {
    val list = MyList.from(0 to 1000000).reverse
    list(0) shouldBe 1000000
    list(1) shouldBe 999999
    list(1000000) shouldBe 0
  }
}
