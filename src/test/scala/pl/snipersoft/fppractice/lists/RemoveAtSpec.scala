package pl.snipersoft.fppractice.lists

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RemoveAtSpec extends AnyFunSuite with Matchers {
  test("should remove element with defined index from list") {
    val list = "a" :: "b" :: "c" :: "d" :: MyNil
    list.removeAt(0) shouldBe "b" :: "c" :: "d" :: MyNil
    list.removeAt(1) shouldBe "a" :: "c" :: "d" :: MyNil
    list.removeAt(2) shouldBe "a" :: "b" :: "d" :: MyNil
    list.removeAt(3) shouldBe "a" :: "b" :: "c" :: MyNil
  }

  test("should return input list if try to remove element with index out of boundary") {
    val list = "a" :: "b" :: "c" :: "d" :: MyNil
    list.removeAt(-1) shouldBe list
    list.removeAt(4) shouldBe list
    list.removeAt(10) shouldBe list
  }

  test("should return empty list if try to remove element from empty list") {
    MyNil.removeAt(-5) shouldBe MyNil
    MyNil.removeAt(-1) shouldBe MyNil
    MyNil.removeAt(0) shouldBe MyNil
    MyNil.removeAt(1) shouldBe MyNil
    MyNil.removeAt(10) shouldBe MyNil
  }

  test("should remove record from big list") {
    val list = MyList.from(0 to 1000000)
    list.removeAt(10)(10) shouldBe 11
  }
}
