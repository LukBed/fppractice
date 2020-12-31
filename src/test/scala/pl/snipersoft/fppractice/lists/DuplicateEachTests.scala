package pl.snipersoft.fppractice.lists

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DuplicateEachTests extends AnyFunSuite with Matchers {
  test("should duplicate each element n times") {
    val list = "a" :: "b" :: "c" :: MyNil
    val expected = "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: "c" :: "c" :: "c" :: MyNil
    list.duplicateEach(3) shouldBe expected
  }
}