package pl.snipersoft.fppractice.lists

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AddAtTheEndSpec extends AnyFunSuite with Matchers {
  test("should add element at the end of multi-elements list") {
    val input = "a" :: "b" :: "c" :: MyNil
    val expected = "a" :: "b" :: "c" :: "d" :: MyNil
    input.addAtTheEnd("d") shouldBe expected
  }

  test("should add element at the end of single element list") {
    val input = "a" :: MyNil
    val expected = "a" :: "b" :: MyNil
    input.addAtTheEnd("b") shouldBe expected
  }

  test("should add element at the end of empty list") {
    MyNil.addAtTheEnd("a") shouldBe "a" :: MyNil
  }
}