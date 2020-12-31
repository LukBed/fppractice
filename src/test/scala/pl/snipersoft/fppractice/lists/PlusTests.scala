package pl.snipersoft.fppractice.lists

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PlusTests extends AnyFunSuite with Matchers {
  test("should add one list to another") {
    val list = "a" :: "b" :: "c" :: MyNil
    val another = "d" :: "e" :: "f" :: MyNil
    val expected = "a" :: "b" :: "c" :: "d" :: "e" :: "f" :: MyNil
    (list ++ another) shouldBe expected
  }
}
