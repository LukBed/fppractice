package pl.snipersoft.fppractice.lists

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LastTests extends AnyFunSuite with Matchers {
  test("should get last element from multi-elements list") {
    val list = "a" :: "b" :: "c" :: MyNil
    list.lastOption shouldBe Some("c")
  }

  test("should get last element from single element list") {
    val list = "a" :: MyNil
    list.lastOption shouldBe Some("a")
  }

  test("should get empty last element from empty list") {
    MyNil.lastOption shouldBe None
  }
}